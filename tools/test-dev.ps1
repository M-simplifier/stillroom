param([string]$OutputDirectory)
$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
if (-not $OutputDirectory) { $OutputDirectory = Join-Path $root ('scratch/tooling-tests/' + [guid]::NewGuid().ToString('N')) }
$OutputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
New-Item -ItemType Directory -Path $OutputDirectory | Out-Null
$savedPath = [Environment]::GetEnvironmentVariable('PATH','Process')
$savedData = [Environment]::GetEnvironmentVariable('stillroom_datadir','Process')
$checks = @()

function Require([bool]$Condition, [string]$Message) { if (-not $Condition) { throw $Message } }
function Reject([string]$Name, [scriptblock]$Work) {
    $rejected = $false
    try { & $Work *> (Join-Path $OutputDirectory "$Name.log") }
    catch { $rejected = $true }
    Require $rejected "$Name should have failed."
}
function Same-Environment {
    Require ([Environment]::GetEnvironmentVariable('PATH','Process') -ceq $script:expectedPath) 'PATH was not restored.'
    Require ([Environment]::GetEnvironmentVariable('stillroom_datadir','Process') -ceq $script:expectedData) 'stillroom_datadir was not restored.'
}
try {
    $env:PATH = $savedPath + ';stillroom-environment-sentinel'
    $env:stillroom_datadir = $root
    $script:expectedPath = $env:PATH
    $script:expectedData = $env:stillroom_datadir
    $absentTool = Join-Path $OutputDirectory 'absent-ghc.exe'
    $badTool = Join-Path $OutputDirectory 'bad-tool'
    Reject 'bad-tool' { & "$PSScriptRoot/dev.ps1" Capture -Ghc $absentTool -OutputDirectory $badTool }
    $report = Get-Content -LiteralPath "$badTool/summary.json" -Raw | ConvertFrom-Json
    Require (-not $report.passed -and $report.failures[0] -like '*Cannot find ghc 9.6.7*') 'Tool failure was not reported.'
    Same-Environment
    $checks += 'invalid explicit tool fails with a summary and restores environment'

    # A caller pointing at all the real source assets must not rescue a package
    # missing its own files. The invalid EXE must never be launched.
    $fixture = Join-Path $OutputDirectory 'incomplete-package'
    New-Item -ItemType Directory -Path $fixture | Out-Null
    $exe = Join-Path $fixture 'stillroom.exe'
    [IO.File]::WriteAllBytes($exe, [byte[]]@(0))
    $missing = Join-Path $OutputDirectory 'missing-files'
    Reject 'missing-files' { & "$PSScriptRoot/verify-native.ps1" -Executable $exe -OutputDirectory $missing }
    $report = Get-Content -LiteralPath "$missing/summary.json" -Raw | ConvertFrom-Json
    Require (-not $report.passed -and $report.assetMode -eq 'package' -and $report.failures[0] -like 'Required files missing:*') 'Package preflight did not reject missing files.'
    Require (-not (Test-Path -LiteralPath "$missing/stdout.txt")) 'An incomplete package was launched.'
    Same-Environment
    $checks += 'package preflight rejects source-asset fallback before launching'

    $nested = Join-Path $OutputDirectory 'nested-failure'
    Reject 'nested-failure' { & "$PSScriptRoot/dev.ps1" CheckPackage -Executable $exe -OutputDirectory $nested }
    $report = Get-Content -LiteralPath "$nested/summary.json" -Raw | ConvertFrom-Json
    Require (-not $report.passed -and $report.checks -eq 0 -and $report.failures[0] -like 'Native acceptance failed*') 'Nested failure was reported as success.'
    Same-Environment
    $checks += 'entrypoint propagates native failure and restores environment'

    $missingExe = Join-Path $OutputDirectory 'missing-exe'
    Reject 'missing-exe' { & "$PSScriptRoot/dev.ps1" CheckPackage -Executable "$fixture/absent.exe" -OutputDirectory $missingExe }
    $report = Get-Content -LiteralPath "$missingExe/summary.json" -Raw | ConvertFrom-Json
    Require (-not $report.passed -and $report.failures.Count -gt 0) 'Missing executable lost its failure report.'
    Same-Environment
    $checks += 'missing executable produces a failure summary'

    $before = (Get-FileHash -LiteralPath "$badTool/summary.json").Hash
    Reject 'preserve-report' { & "$PSScriptRoot/dev.ps1" Capture -Ghc $absentTool -OutputDirectory $badTool }
    Require ((Get-FileHash -LiteralPath "$badTool/summary.json").Hash -eq $before) 'An existing report was overwritten.'
    Same-Environment
    $checks += 'rerun preserves existing evidence'

    Remove-Item -LiteralPath Env:stillroom_datadir -ErrorAction SilentlyContinue
    $script:expectedData = [Environment]::GetEnvironmentVariable('stillroom_datadir','Process')
    Reject 'unset-data' { & "$PSScriptRoot/verify-native.ps1" -Executable $exe -OutputDirectory "$OutputDirectory/unset-data" }
    Same-Environment
    $checks += 'an initially absent data override stays absent after failure'
} finally {
    $restore = @{PATH=$savedPath; stillroom_datadir=$savedData}
    foreach ($name in $restore.Keys) {
        if ($null -eq $restore[$name]) { Remove-Item -LiteralPath "Env:$name" -ErrorAction SilentlyContinue }
        else { [Environment]::SetEnvironmentVariable($name,$restore[$name],'Process') }
    }
}
[ordered]@{passed=$true; checks=$checks.Count; cases=$checks; evidence=$OutputDirectory} |
    ConvertTo-Json -Depth 4 | Set-Content -LiteralPath "$OutputDirectory/summary.json" -Encoding UTF8
[ordered]@{passed=$true; checks=$checks.Count; report="$OutputDirectory/summary.json"} | ConvertTo-Json -Compress
