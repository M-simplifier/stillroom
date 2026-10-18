param(
    [Parameter(Mandatory=$true)][string]$Executable,
    [string]$OutputDirectory = (Join-Path $PSScriptRoot '../native-check'),
    [string]$MusicFolder,
    [string]$DevelopmentAssetRoot
)
$ErrorActionPreference = 'Stop'
$OutputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
New-Item -ItemType Directory -Force -Path $OutputDirectory | Out-Null
$reportFile = Join-Path $OutputDirectory 'native-check.json'
$summaryFile = Join-Path $OutputDirectory 'summary.json'
if ((Test-Path -LiteralPath $reportFile) -or (Test-Path -LiteralPath $summaryFile)) {
    throw 'Choose a fresh output directory to preserve the previous run.'
}
$savedPath = [Environment]::GetEnvironmentVariable('PATH','Process')
$savedData = [Environment]::GetEnvironmentVariable('stillroom_datadir','Process')
$timer = [Diagnostics.Stopwatch]::StartNew()
$result = [ordered]@{passed=$false; executable=$Executable; executableSHA256=$null; assetMode='package'; checks=0; failures=@(); report=$reportFile}

# --check uses a hidden, muted window and its own preferences. It still needs
# a real graphics context and an available audio device.
try {
    $Executable = (Resolve-Path -LiteralPath $Executable).Path
    $result.executable = $Executable
    $result.executableSHA256 = (Get-FileHash -LiteralPath $Executable -Algorithm SHA256).Hash.ToLowerInvariant()
    $binaryRoot = Split-Path -Parent $Executable
    $assetRoot = $binaryRoot
    if ($DevelopmentAssetRoot) {
        $assetRoot = (Resolve-Path -LiteralPath $DevelopmentAssetRoot).Path
        $result.assetMode = 'development'
        $env:stillroom_datadir = $assetRoot
    } else {
        # A package must stand alone, even when its caller has a working
        # development checkout or a convenient DLL elsewhere on PATH.
        Remove-Item -LiteralPath Env:stillroom_datadir -ErrorAction SilentlyContinue
        $env:PATH = (Join-Path $env:SystemRoot 'System32') + ';' + $env:SystemRoot
    }
    $required = @((Join-Path $binaryRoot 'libgmp-10.dll'))
    foreach ($file in @(
        'fonts/NotoSansJP.otf','fonts/OFL.txt','rest/procedural.fs',
        'rest/nature/common.glsl','rest/nature/land.vs','rest/nature/land.fs',
        'rest/nature/water.vs','rest/nature/water.fs','rest/nature/sky.fs','rest/nature/post.fs',
        'rest/journey/common.glsl','rest/journey/land.vs','rest/journey/land.fs','rest/journey/water.vs',
        'rest/journey/water.fs','rest/journey/sky.fs','rest/journey/post.fs','rest/journey/poster.png',
        'backgrounds/01-mist-lake.png','backgrounds/02-quiet-forest.png',
        'backgrounds/03-soft-dunes.png','backgrounds/04-night-field.png',
        'backgrounds/05-afternoon-lake.png','backgrounds/06-woodland-pond.png','icon/stillroom.png'
    )) { $required += Join-Path $assetRoot "assets/$file" }
    $missing = @($required | Where-Object { -not (Test-Path -LiteralPath $_ -PathType Leaf) })
    if ($missing.Count -gt 0) { throw ('Required files missing: ' + ($missing -join ', ')) }
    $arguments = @('--check', ('"' + $OutputDirectory + '"'))
    if ($MusicFolder) {
        $arguments += @('--music', ('"' + [IO.Path]::GetFullPath($MusicFolder) + '"'))
    }
    $process = Start-Process -FilePath $Executable -ArgumentList $arguments `
        -WorkingDirectory $binaryRoot -WindowStyle Hidden `
        -RedirectStandardOutput (Join-Path $OutputDirectory 'stdout.txt') `
        -RedirectStandardError (Join-Path $OutputDirectory 'stderr.txt') -PassThru
    try {
        $nativeTimer = [Diagnostics.Stopwatch]::StartNew()
        $peakWorkingSet = 0L
        while (-not $process.WaitForExit(200)) {
            $process.Refresh()
            if (-not $process.HasExited) { $peakWorkingSet = [Math]::Max($peakWorkingSet, $process.PeakWorkingSet64) }
            if ($nativeTimer.Elapsed.TotalSeconds -gt 120) {
                Stop-Process -Id $process.Id
                throw 'Native acceptance timed out.'
            }
        }
        $result.peakWorkingSetBytes = $peakWorkingSet
        $process.Refresh()
        if ($process.ExitCode -ne 0) {
            throw "Native acceptance failed with exit code $($process.ExitCode). See $OutputDirectory."
        }
    } finally { $process.Dispose() }
    $report = Get-Content -LiteralPath $reportFile -Raw -Encoding UTF8 | ConvertFrom-Json
    $result.checks = @($report.checks).Count
    $result.failures = @($report.checks | Where-Object { -not $_.passed } | ForEach-Object { $_.name })
    if (-not $report.passed -or $result.checks -ne 56 -or $result.failures.Count -gt 0) {
        throw 'Native acceptance is incomplete or failed.'
    }
    $result.passed = $true
} catch {
    $result.failures += $_.Exception.Message
} finally {
    $restore = @{PATH=$savedPath; stillroom_datadir=$savedData}
    foreach ($name in $restore.Keys) {
        if ($null -eq $restore[$name]) { Remove-Item -LiteralPath "Env:$name" -ErrorAction SilentlyContinue }
        else { [Environment]::SetEnvironmentVariable($name,$restore[$name],'Process') }
    }
    $result.elapsedSeconds = $timer.Elapsed.TotalSeconds
    $result | ConvertTo-Json -Depth 4 | Set-Content -LiteralPath $summaryFile -Encoding UTF8
}
[ordered]@{passed=$result.passed; checks=$result.checks; failures=$result.failures; report=$summaryFile} | ConvertTo-Json -Compress
if (-not $result.passed) { throw "Native acceptance failed. See $summaryFile" }
