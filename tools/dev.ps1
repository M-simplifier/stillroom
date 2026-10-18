param(
    [Parameter(Position=0)][ValidateSet('Build','Capture','Check','CheckPackage','Benchmark')][string]$Task = 'Build',
    [string]$OutputDirectory,
    [string]$Executable,
    [string]$MusicFolder,
    [string]$Ghc,
    [string]$Cabal,
    [switch]$Online
)
$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
if (-not $OutputDirectory) {
    $run = (Get-Date -Format 'yyyyMMdd-HHmmss-fff') + '-' + $Task.ToLowerInvariant() + '-' + [guid]::NewGuid().ToString('N').Substring(0,8)
    $OutputDirectory = Join-Path $root "scratch/dev/$run"
}
$OutputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
New-Item -ItemType Directory -Force -Path $OutputDirectory | Out-Null
$summaryFile = Join-Path $OutputDirectory 'summary.json'
if (Test-Path -LiteralPath $summaryFile) { throw 'Choose a fresh output directory to preserve the previous run.' }
$savedPath = [Environment]::GetEnvironmentVariable('PATH','Process')
$savedData = [Environment]::GetEnvironmentVariable('stillroom_datadir','Process')
$timer = [Diagnostics.Stopwatch]::StartNew()
$result = [ordered]@{ task=$Task; passed=$false; repositoryCommit=$null; dirty=$null; executable=$null; executableSHA256=$null; checks=0; captures=0; failures=@(); stages=@(); report=$summaryFile }

function Find-PinnedTool([string]$Explicit, [string]$Name, [string]$Version) {
    $candidates = @()
    if ($Explicit) {
        $candidates += $Explicit
    } else {
        $bases = @((Join-Path $env:SystemDrive 'ghcup'), (Join-Path $env:USERPROFILE '.ghcup'))
        if ($env:GHCUP_INSTALL_BASE_PREFIX) {
            $bases = @($env:GHCUP_INSTALL_BASE_PREFIX, (Join-Path $env:GHCUP_INSTALL_BASE_PREFIX 'ghcup')) + $bases
        }
        foreach ($base in $bases) {
            if ($Name -eq 'ghc') { $candidates += Join-Path $base "ghc/$Version/bin/ghc.exe" }
            else { $candidates += Join-Path $base "bin/cabal-$Version.exe" }
        }
        $command = Get-Command "$Name.exe" -ErrorAction SilentlyContinue
        if ($command) { $candidates += $command.Source }
    }
    foreach ($candidate in $candidates) {
        if (Test-Path -LiteralPath $candidate -PathType Leaf) {
            $path = (Resolve-Path -LiteralPath $candidate).Path
            $actual = (& $path --numeric-version 2>$null | Out-String).Trim()
            if ($LASTEXITCODE -eq 0 -and $actual -eq $Version) { return $path }
        }
    }
    throw "Cannot find $Name $Version. Install the pinned toolchain or pass -$Name with its executable path."
}

function Invoke-Logged([string]$Name, [scriptblock]$Work) {
    $log = Join-Path $OutputDirectory "$Name.log"
    $clock = [Diagnostics.Stopwatch]::StartNew()
    try { & $Work *> $log }
    finally { $result.stages += [ordered]@{name=$Name; seconds=$clock.Elapsed.TotalSeconds; log=$log} }
}

function Invoke-Inspection([string]$Mode, [string]$ReportName) {
    $folder = Join-Path $OutputDirectory $Mode
    New-Item -ItemType Directory -Path $folder | Out-Null
    $arguments = @("--$Mode", ('"' + $folder + '"'))
    if ($Mode -eq 'benchmark') { $arguments += @('+RTS','-T','-RTS') }
    $process = Start-Process -FilePath $Executable -ArgumentList $arguments -WindowStyle Hidden -PassThru `
        -WorkingDirectory (Split-Path -Parent $Executable) `
        -RedirectStandardOutput (Join-Path $folder 'stdout.log') -RedirectStandardError (Join-Path $folder 'stderr.log')
    try {
        if (-not $process.WaitForExit(59000)) { Stop-Process -Id $process.Id; throw "$Mode timed out. See $folder" }
        $process.Refresh()
        if ($process.ExitCode -ne 0) { throw "$Mode exited with $($process.ExitCode). See $folder/stderr.log" }
    } finally { $process.Dispose() }
    $report = Get-Content -LiteralPath (Join-Path $folder $ReportName) -Raw -Encoding UTF8 | ConvertFrom-Json
    if ($Mode -eq 'capture-ui') {
        if (-not $report.completed -or $report.count -ne 22) { throw 'UI capture is incomplete.' }
        foreach ($capture in $report.captures) {
            if (-not (Test-Path -LiteralPath (Join-Path $folder $capture.file) -PathType Leaf)) { throw "Capture missing: $($capture.file)" }
        }
        $result.captures = $report.count
    } else {
        if ($report.results.Count -ne 18 -or $report.landscapeResults.Count -ne 36) { throw 'Renderer benchmark is incomplete.' }
        foreach ($check in $report.landscapeContracts.PSObject.Properties) {
            if ($check.Value -ne $true) { throw "Landscape cache failed: $($check.Name)" }
        }
        $result.checks = $report.results.Count + $report.landscapeResults.Count
    }
}

try {
    $git = Get-Command git.exe -ErrorAction SilentlyContinue
    if ($git) {
        $commit = & $git.Source -C $root rev-parse HEAD 2>$null
        if ($LASTEXITCODE -eq 0) {
            $result.repositoryCommit = "$commit".Trim()
            $result.dirty = [bool](& $git.Source -C $root status --porcelain)
        }
    }
    if ($Task -eq 'CheckPackage') {
        if (-not $Executable) { throw 'CheckPackage requires -Executable from an extracted distribution.' }
        if ($Online -or $Ghc -or $Cabal) { throw 'CheckPackage does not use a development toolchain.' }
        $Executable = (Resolve-Path -LiteralPath $Executable).Path
        $result.assetMode = 'package'
    } else {
        if ($Executable) { throw '-Executable is only accepted with CheckPackage; development tasks build the current source.' }
        $Ghc = Find-PinnedTool $Ghc 'ghc' '9.6.7'
        $Cabal = Find-PinnedTool $Cabal 'cabal' '3.12.1.0'
        $env:PATH = (Split-Path -Parent $Ghc) + ';' + (Split-Path -Parent $Cabal) + ';' + $savedPath
        $result.toolchain = @{ghc=$Ghc; cabal=$Cabal}
        $result.assetMode = 'development'
        Invoke-Logged 'build' {
            & (Join-Path $PSScriptRoot 'build.ps1') -Cabal $Cabal -Offline:(-not $Online) -SkipTests:($Task -ne 'Build')
        }
        $plan = Get-Content -LiteralPath (Join-Path $root 'dist-newstyle/cache/plan.json') -Raw -Encoding UTF8 | ConvertFrom-Json
        $Executable = ($plan.'install-plan' | Where-Object { $_.'component-name' -eq 'exe:stillroom' }).'bin-file'
        if (-not $Executable -or -not (Test-Path -LiteralPath $Executable)) { throw 'Build plan has no executable.' }
        $env:stillroom_datadir = $root
        $env:PATH = (Join-Path $root 'vendor/gmp/ucrt64/bin') + ';' + $env:PATH
    }
    $result.executable = $Executable
    $result.executableSHA256 = (Get-FileHash -LiteralPath $Executable -Algorithm SHA256).Hash.ToLowerInvariant()
    switch ($Task) {
        'Build' {
            $result.checks = @(Select-String -LiteralPath (Join-Path $OutputDirectory 'build.log') -Pattern '^\+\+\+ OK, passed').Count
            if ($result.checks -eq 0) { throw 'Build returned without law-test results.' }
        }
        'Capture' { Invoke-Logged 'capture' { Invoke-Inspection 'capture-ui' 'ui-capture.json' } }
        'Benchmark' { Invoke-Logged 'benchmark' { Invoke-Inspection 'benchmark' 'renderer-benchmark.json' } }
        { $_ -in @('Check','CheckPackage') } {
            $options = @{Executable=$Executable; OutputDirectory=(Join-Path $OutputDirectory 'native')}
            if ($MusicFolder) { $options.MusicFolder=$MusicFolder }
            if ($Task -eq 'Check') { $options.DevelopmentAssetRoot=$root }
            Invoke-Logged 'native' { & (Join-Path $PSScriptRoot 'verify-native.ps1') @options }
            $native = Get-Content -LiteralPath (Join-Path $options.OutputDirectory 'summary.json') -Raw -Encoding UTF8 | ConvertFrom-Json
            $result.checks = $native.checks
        }
    }
    $result.passed = $true
} catch {
    $result.failures = @($_.Exception.Message)
} finally {
    $restore = @{PATH=$savedPath; stillroom_datadir=$savedData}
    foreach ($name in $restore.Keys) {
        if ($null -eq $restore[$name]) { Remove-Item -LiteralPath "Env:$name" -ErrorAction SilentlyContinue }
        else { [Environment]::SetEnvironmentVariable($name,$restore[$name],'Process') }
    }
    $result.elapsedSeconds = $timer.Elapsed.TotalSeconds
    $result | ConvertTo-Json -Depth 8 | Set-Content -LiteralPath $summaryFile -Encoding UTF8
}
[ordered]@{task=$Task; passed=$result.passed; commit=$result.repositoryCommit; dirty=$result.dirty; artifactSHA256=$result.executableSHA256; checks=$result.checks; captures=$result.captures; failures=$result.failures; report=$summaryFile} | ConvertTo-Json -Compress -Depth 4
if (-not $result.passed) { throw "Stillroom $Task failed. See $summaryFile" }
