param(
    [ValidateSet('Cpu','Native')][string]$Task = 'Cpu',
    [string]$OutputDirectory,
    [string]$ReferenceGeometryDirectory,
    [string]$Executable,
    [string]$AssetRoot,
    [string]$Ghc = 'C:/ghcup/ghc/9.6.7/bin/ghc.exe',
    [string]$Cabal = 'C:/ghcup/bin/cabal-3.12.1.0.exe',
    [string]$Python = 'C:/Python314/python.exe'
)
$ErrorActionPreference = 'Stop'
$repositoryRoot = Split-Path -Parent $PSScriptRoot
if (-not $OutputDirectory) {
    $runName = (Get-Date -Format 'yyyyMMdd-HHmmss-fff') + '-' + $Task.ToLowerInvariant()
    $OutputDirectory = Join-Path $repositoryRoot "scratch/haskell-migration/$runName"
}
$OutputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath (Join-Path $OutputDirectory 'summary.json')) { throw 'Use a fresh output directory.' }
New-Item -ItemType Directory -Force -Path $OutputDirectory | Out-Null
$report = [ordered]@{ task=$Task; passed=$false; stages=@(); failures=@(); output=$OutputDirectory }
$savedData = [Environment]::GetEnvironmentVariable('stillroom_datadir','Process')
$savedPath = [Environment]::GetEnvironmentVariable('PATH','Process')

function Invoke-Stage([string]$Name, [scriptblock]$Work) {
    $timer = [Diagnostics.Stopwatch]::StartNew()
    $logPath = Join-Path $OutputDirectory "$Name.log"
    try { & $Work *> $logPath }
    finally { $report.stages += [ordered]@{ name=$Name; elapsedSeconds=$timer.Elapsed.TotalSeconds; log=$logPath } }
}

try {
    Push-Location $repositoryRoot
    $report.sourceRevision = (& git rev-parse HEAD).Trim()
    $report.dirty = [bool](& git status --porcelain)
    if ($Task -eq 'Cpu') {
        if ((& $Ghc --numeric-version).Trim() -ne '9.6.7') { throw 'The migration comparison requires pinned GHC 9.6.7.' }
        $cabalPaths = (& $Cabal path --output-format=json | Out-String | ConvertFrom-Json)
        $packageDatabase = Join-Path $cabalPaths.'store-dir' 'ghc-9.6.7/package.db'
        $compilerRoot = Split-Path -Parent (Split-Path -Parent $Ghc)
        $clang = Join-Path $compilerRoot 'mingw/bin/clang.exe'
        $report.compiler = $Ghc
        $report.optimization = '-O2; threaded runtime; N2; signature work included before generation timing ends'
        if (-not $ReferenceGeometryDirectory) {
            $referenceRoot = Join-Path $OutputDirectory 'reference-source'
            $referenceArchive = Join-Path $OutputDirectory 'reference-source.zip'
            & git archive --format=zip --output $referenceArchive f72f2420ab91956b9ba76998899971e0b9ab82d2 cbits
            if ($LASTEXITCODE) { throw 'Frozen reference source acquisition failed.' }
            Expand-Archive -LiteralPath $referenceArchive -DestinationPath $referenceRoot
            $ReferenceGeometryDirectory = Join-Path $OutputDirectory 'reference-geometry'
            Invoke-Stage 'reference' {
                & $Python (Join-Path $PSScriptRoot 'migration-reference-geometry.py') --source-root $referenceRoot --output $ReferenceGeometryDirectory --clang $clang
                if ($LASTEXITCODE) { throw 'Native CPU reference failed.' }
            }
        }
        $ReferenceGeometryDirectory = (Resolve-Path -LiteralPath $ReferenceGeometryDirectory).Path
        $fieldExe = Join-Path $OutputDirectory 'field.exe'
        Invoke-Stage 'field' {
            & $Ghc -O2 -XGHC2021 -fno-embed-manifest -isrc -outputdir (Join-Path $OutputDirectory 'field-build') test/MigrationFieldMain.hs -o $fieldExe
            if ($LASTEXITCODE) { throw 'Field harness compilation failed.' }
            & $fieldExe
            if ($LASTEXITCODE) { throw 'Independent field equivalence failed.' }
        }
        Invoke-Stage 'opaque-types' {
            foreach ($case in @(@{name='ValidGeometry';field=$null},@{name='RejectGeometryWidth';field='heightWidth'},@{name='RejectMeshPositions';field='meshPositions'})) {
                $typeLog = Join-Path $OutputDirectory ($case.name + '.log')
                & $Ghc -XGHC2021 -fno-code -fforce-recomp -package-env - -isrc -package-db $packageDatabase -package vector -outputdir (Join-Path $OutputDirectory 'type-build') ("test/types/"+$case.name+".hs") *> $typeLog
                if ($null -eq $case.field) {
                    if ($LASTEXITCODE) { throw "Positive opaque API client failed. See $typeLog" }
                } else {
                    if ($LASTEXITCODE -eq 0) { throw "Opaque API accepted an invalid update: $($case.name)" }
                    if (-not (Select-String -LiteralPath $typeLog -SimpleMatch 'GHC-47535' -Quiet) -or -not (Select-String -LiteralPath $typeLog -SimpleMatch $case.field -Quiet)) {
                        throw "Opaque API failed for an unrelated reason. See $typeLog"
                    }
                }
            }
            Write-Output 'Opaque geometry: positive client accepted; both invalid record updates rejected.'
        }
        $geometryExe = Join-Path $OutputDirectory 'geometry.exe'
        $actualGeometry = Join-Path $OutputDirectory 'geometry'
        $actualCsv = Join-Path $OutputDirectory 'geometry.csv'
        Invoke-Stage 'geometry' {
            & $Ghc -O2 -XGHC2021 -fno-embed-manifest -threaded -rtsopts -with-rtsopts=-N2 -isrc -package-db $packageDatabase -package vector -package bytestring -outputdir (Join-Path $OutputDirectory 'geometry-build') test/MigrationGeometryMain.hs -o $geometryExe
            if ($LASTEXITCODE) { throw 'Geometry harness compilation failed.' }
            & $geometryExe $actualGeometry +RTS -T -RTS > $actualCsv
            if ($LASTEXITCODE) { throw 'Geometry generation failed.' }
            & $Python (Join-Path $PSScriptRoot 'migration-compare-geometry.py') --reference $ReferenceGeometryDirectory --actual $actualGeometry --actual-csv $actualCsv --output (Join-Path $OutputDirectory 'geometry-comparison.json')
            if ($LASTEXITCODE) { throw 'Independent geometry equivalence failed.' }
        }
    } else {
        if (-not $Executable) { throw 'Native comparison requires an already built executable.' }
        $Executable = (Resolve-Path -LiteralPath $Executable).Path
        if (-not $AssetRoot) { $AssetRoot = $repositoryRoot }
        $env:stillroom_datadir = (Resolve-Path -LiteralPath $AssetRoot).Path
        $env:PATH = (Join-Path $repositoryRoot 'vendor/gmp/ucrt64/bin') + ';' + $savedPath
        $report.executable = $Executable
        $report.executableSHA256 = (Get-FileHash -Algorithm SHA256 -LiteralPath $Executable).Hash.ToLowerInvariant()
        $report.personalSettingsRead = $false
        $report.modes = @()
        foreach ($mode in @('quality-capture','capture-ui','check-journey','check-graphics','benchmark','check')) {
            $folder = Join-Path $OutputDirectory $mode
            New-Item -ItemType Directory -Path $folder | Out-Null
            $arguments = @("--$mode", ('"' + $folder + '"'))
            if ($mode -eq 'benchmark') { $arguments += @('+RTS','-T','-RTS') }
            $timer = [Diagnostics.Stopwatch]::StartNew()
            $process = Start-Process -FilePath $Executable -ArgumentList $arguments -WorkingDirectory (Split-Path -Parent $Executable) -WindowStyle Hidden -PassThru -RedirectStandardOutput (Join-Path $folder 'stdout.log') -RedirectStandardError (Join-Path $folder 'stderr.log')
            $peakWorkingSet = 0L
            try {
                while (-not $process.WaitForExit(100)) {
                    $process.Refresh()
                    $peakWorkingSet = [Math]::Max($peakWorkingSet,$process.WorkingSet64)
                    if ($timer.Elapsed.TotalSeconds -gt 180) { Stop-Process -Id $process.Id; throw "$mode timed out." }
                }
                $process.Refresh()
                if ($process.ExitCode -ne 0) { throw "$mode failed with exit code $($process.ExitCode)." }
            } finally { $process.Dispose() }
            $filename = switch ($mode) {
                'quality-capture' {'quality-check.json'}
                'capture-ui' {'ui-capture.json'}
                'check-journey' {'journey-check.json'}
                'check-graphics' {'graphics-check.json'}
                'benchmark' {'renderer-benchmark.json'}
                'check' {'native-check.json'}
            }
            $result = Get-Content -Raw -Encoding UTF8 (Join-Path $folder $filename) | ConvertFrom-Json
            if ($mode -eq 'capture-ui') {
                if (-not $result.completed -or $result.count -ne 32) { throw 'Incomplete UI captures.' }
            } elseif ($mode -eq 'benchmark') {
                if ($result.results.Count -ne 18 -or $result.landscapeResults.Count -ne 36) { throw 'Incomplete renderer benchmark.' }
                foreach ($property in $result.landscapeContracts.PSObject.Properties) {
                    if ($property.Value -ne $true) { throw "Renderer cache contract failed: $($property.Name)" }
                }
            } elseif (-not $result.passed) { throw "$mode did not report success." }
            $report.modes += [ordered]@{ mode=$mode; elapsedSeconds=$timer.Elapsed.TotalSeconds; peakWorkingSetBytes=$peakWorkingSet; screenshots=@(Get-ChildItem -LiteralPath $folder -Filter '*.png').Count; report=(Join-Path $folder $filename) }
            $report | ConvertTo-Json -Depth 8 | Set-Content -LiteralPath (Join-Path $OutputDirectory 'running-summary.json') -Encoding UTF8
        }
    }
    $report.passed = $true
} catch {
    $report.failures += $_.Exception.Message
} finally {
    Pop-Location
    [Environment]::SetEnvironmentVariable('stillroom_datadir',$savedData,'Process')
    [Environment]::SetEnvironmentVariable('PATH',$savedPath,'Process')
    $report | ConvertTo-Json -Depth 8 | Set-Content -LiteralPath (Join-Path $OutputDirectory 'summary.json') -Encoding UTF8
}
[ordered]@{task=$Task;passed=$report.passed;failures=$report.failures;report=(Join-Path $OutputDirectory 'summary.json')} | ConvertTo-Json -Compress
if (-not $report.passed) { throw 'Haskell migration comparison failed; see summary.json.' }
