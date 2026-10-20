param([string]$Ghc = 'ghc')
$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
$folder = Join-Path $root ('scratch/type-checks/' + [guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $folder -Force | Out-Null
Push-Location $root
try {
    # A positive control distinguishes an API rejection from a broken compiler
    # invocation, missing source, or unavailable import.
    $cases = @(
        @{name='Valid'; error=$null},
        @{name='ReplacePlan'; error='GHC-47535'},
        @{name='ReplaceObservation'; error='GHC-47535'},
        @{name='CoerceSpace'; error="Couldn't match type"},
        @{name='ComposeSpaces'; error="Couldn't match type"}
    )
    foreach ($case in $cases) {
        $log = Join-Path $folder ($case.name + '.log')
        & $Ghc -XGHC2021 -fno-code -fforce-recomp -package-env - -isrc `
            -outputdir $folder ("test/types/" + $case.name + ".hs") *> $log
        $code = $LASTEXITCODE
        if ($null -eq $case.error) {
            if ($code -ne 0) { throw "Positive type-check failed. See $log" }
        } else {
            if ($code -eq 0) { throw "Invalid graphics client compiled: $($case.name)" }
            if (-not (Select-String -LiteralPath $log -SimpleMatch $case.error -Quiet)) {
                throw "Unexpected compilation failure. See $log"
            }
        }
    }
    Write-Output 'Graphics API: positive client accepted; four invalid clients rejected.'
} finally { Pop-Location }
