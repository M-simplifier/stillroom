param([string]$Cabal = 'cabal', [string]$OutputDirectory = (Join-Path $PSScriptRoot '../release'))
$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
$OutputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
Push-Location $root
try {
    $plan = Get-Content -LiteralPath (Join-Path $root 'dist-newstyle/cache/plan.json') -Raw -Encoding UTF8 | ConvertFrom-Json
    $executable = ($plan.'install-plan' | Where-Object { $_.'component-name' -eq 'exe:stillroom' }).'bin-file'
    if (-not $executable -or -not (Test-Path -LiteralPath $executable)) { throw 'Build the executable first.' }
    $ghcLib = (& ghc --print-libdir).Trim()
    $objdump = Join-Path (Split-Path -Parent $ghcLib) 'mingw/bin/objdump.exe'
    $imports = & $objdump -p $executable
    if ($LASTEXITCODE -or -not ($imports | Select-String 'DLL Name:\s+libgmp-10\.dll')) {
        throw 'Use tools/build.ps1 to produce an executable with replaceable GMP.'
    }
    $gmp = Join-Path $root 'vendor/gmp'
    $dll = Join-Path (Split-Path -Parent $executable) 'libgmp-10.dll'
    # Releases pair this exact MSYS2 DLL with its corresponding source. Users
    # can replace their own DLL, but a different binary needs its own sources.
    $pinned = @{
        $dll = 'c2567ccf3f410c3411ba7cd4b7ba14f393a0e70a1d0c0593297df6706b5fba96'
        (Join-Path $gmp 'gmp.src.tar.zst') = 'f288f944fd9609db220bcf6a8dd0703a5674eeb906ef35eb8485bb8192135994'
        (Join-Path $gmp '.BUILDINFO') = 'ac4a5faeb55fa4fb1a6bcb10773b60ab6fe7683f7707f2f05ecc76b9c8d06398'
    }
    foreach ($file in $pinned.Keys) {
        if (-not (Test-Path -LiteralPath $file) -or
            (Get-FileHash -LiteralPath $file -Algorithm SHA256).Hash.ToLowerInvariant() -ne $pinned[$file]) {
            throw 'GMP binary/source mismatch. Run tools/build.ps1 before packaging.'
        }
    }
    $bundle = Join-Path $OutputDirectory 'stillroom-0.8.0-windows-x64'
    if (Test-Path -LiteralPath $bundle) { throw "Choose an empty destination: $bundle" }
    New-Item -ItemType Directory -Force -Path (Join-Path $bundle 'assets/fonts') | Out-Null
    Copy-Item -LiteralPath $executable -Destination (Join-Path $bundle 'stillroom.exe')
    Copy-Item -LiteralPath $dll -Destination $bundle
    New-Item -ItemType Directory -Path (Join-Path $bundle 'third-party-source') | Out-Null
    Copy-Item -LiteralPath (Join-Path $gmp 'gmp.src.tar.zst') -Destination (Join-Path $bundle 'third-party-source/gmp-6.3.0-2.src.tar.zst')
    Copy-Item -LiteralPath (Join-Path $gmp '.BUILDINFO') -Destination (Join-Path $bundle 'third-party-source/gmp-BUILDINFO.txt')
    foreach ($name in @('NotoSansJP.otf','OFL.txt')) {
        Copy-Item -LiteralPath (Join-Path $root "assets/fonts/$name") -Destination (Join-Path $bundle 'assets/fonts')
    }
    Copy-Item -LiteralPath (Join-Path $root 'assets/backgrounds') -Destination (Join-Path $bundle 'assets') -Recurse
    Copy-Item -LiteralPath (Join-Path $root 'assets/icon') -Destination (Join-Path $bundle 'assets') -Recurse
    New-Item -ItemType Directory -Path (Join-Path $bundle 'assets/rest') | Out-Null
    Copy-Item -LiteralPath (Join-Path $root 'assets/rest/procedural.fs') -Destination (Join-Path $bundle 'assets/rest')
    Copy-Item -LiteralPath (Join-Path $root 'assets/rest/nature') -Destination (Join-Path $bundle 'assets/rest') -Recurse
    Copy-Item -LiteralPath (Join-Path $root 'assets/rest/journey') -Destination (Join-Path $bundle 'assets/rest') -Recurse
    Copy-Item -LiteralPath (Join-Path $root 'licenses') -Destination $bundle -Recurse
    foreach ($name in @('LICENSE','README.md','STATUS.md','CONTRIBUTING.md','MAINTAINERS.md','CHANGELOG.md')) {
        Copy-Item -LiteralPath (Join-Path $root $name) -Destination $bundle
    }
    New-Item -ItemType Directory -Path (Join-Path $bundle 'docs') | Out-Null
    foreach ($name in @('guide.md','rest-mode.md','journeys.md','shader-research.md','procedural-landscapes.md','music-licensing.md','assets.md','background-prompts.md','architecture.md','performance.md','design.md','development.md','gmp.md','player.png','rest-gallery.png','rest-water.png','rest-landscape.png','journey.png')) {
        Copy-Item -LiteralPath (Join-Path $root "docs/$name") -Destination (Join-Path $bundle 'docs')
    }
    $archiveName = (Split-Path -Leaf $bundle) + '.zip'
    $archive = Join-Path $OutputDirectory $archiveName
    if (Test-Path -LiteralPath $archive) { throw 'Archive already exists.' }
    Compress-Archive -LiteralPath $bundle -DestinationPath $archive -CompressionLevel Optimal
    $hash = (Get-FileHash -LiteralPath $archive -Algorithm SHA256).Hash.ToLowerInvariant()
    [IO.File]::WriteAllText((Join-Path $OutputDirectory 'SHA256SUMS.txt'), "$hash  $archiveName`n")
    Write-Output $archive
} finally { Pop-Location }
