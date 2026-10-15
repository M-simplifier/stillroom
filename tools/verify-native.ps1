param(
    [Parameter(Mandatory=$true)][string]$Executable,
    [string]$OutputDirectory = (Join-Path $PSScriptRoot '../native-check'),
    [string]$MusicFolder
)
$ErrorActionPreference = 'Stop'
$Executable = (Resolve-Path -LiteralPath $Executable).Path
$OutputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
New-Item -ItemType Directory -Force -Path $OutputDirectory | Out-Null
$reportFile = Join-Path $OutputDirectory 'native-check.json'
if (Test-Path -LiteralPath $reportFile) { Remove-Item -LiteralPath $reportFile }

# --check uses a hidden, muted window and its own preferences. It still needs
# a real graphics context and an available audio device.
$arguments = @('--check', ('"' + $OutputDirectory + '"'))
if ($MusicFolder) {
    $arguments += @('--music', ('"' + [IO.Path]::GetFullPath($MusicFolder) + '"'))
}
$process = Start-Process -FilePath $Executable -ArgumentList $arguments `
    -WorkingDirectory (Split-Path -Parent $Executable) -WindowStyle Hidden `
    -RedirectStandardOutput (Join-Path $OutputDirectory 'stdout.txt') `
    -RedirectStandardError (Join-Path $OutputDirectory 'stderr.txt') -PassThru
if (-not $process.WaitForExit(59000)) {
    Stop-Process -Id $process.Id
    throw 'Native acceptance timed out.'
}
$process.Refresh()
if ($process.ExitCode -ne 0) {
    throw "Native acceptance failed with exit code $($process.ExitCode). See $OutputDirectory."
}
$report = Get-Content -LiteralPath $reportFile -Raw | ConvertFrom-Json
if (-not $report.passed -or $report.checks.Count -ne 42) { throw 'Native acceptance is incomplete or failed.' }
$report.checks | Format-Table name, passed
