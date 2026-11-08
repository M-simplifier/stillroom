param(
    [string]$Cabal = 'cabal',
    [string]$Ghc = 'ghc',
    [string]$BuildDirectory = 'dist-newstyle',
    [string]$OutputDirectory = (Join-Path $PSScriptRoot '../release'),
    [switch]$LocalReview
)
$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
$OutputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
Push-Location $root
try {
    $plan = Get-Content -LiteralPath (Join-Path $BuildDirectory 'cache/plan.json') -Raw -Encoding UTF8 | ConvertFrom-Json
    $app = $plan.'install-plan' | Where-Object { $_.'component-name' -eq 'exe:stillroom' }
    $executable = $app.'bin-file'
    $worker = ($plan.'install-plan' | Where-Object { $_.'component-name' -eq 'exe:gameio-audio-worker' }).'bin-file'
    $ghcLib = (& $Ghc --print-libdir).Trim()
    $objdump = Join-Path (Split-Path -Parent $ghcLib) 'mingw/bin/objdump.exe'
    foreach ($binary in @($executable, $worker)) {
        if (-not $binary -or -not (Test-Path -LiteralPath $binary -PathType Leaf)) { throw 'Build the executable and its audio worker first.' }
        $imports = & $objdump -p $binary
        if ($LASTEXITCODE -or -not ($imports | Select-String 'DLL Name:\s+libgmp-10\.dll')) {
            throw "Use tools/build.ps1 to link against replaceable GMP: $binary"
        }
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
    $version = $app.'pkg-version' -replace '^(\d+\.\d+\.\d+)\.0$', '$1'
    $bundle = Join-Path $OutputDirectory "stillroom-$version-windows-x64"
    if (Test-Path -LiteralPath $bundle) { throw "Choose an empty destination: $bundle" }
    New-Item -ItemType Directory -Force -Path (Join-Path $bundle 'assets/fonts') | Out-Null
    Copy-Item -LiteralPath $executable -Destination (Join-Path $bundle 'stillroom.exe')
    Copy-Item -LiteralPath $worker -Destination (Join-Path $bundle 'gameio-audio-worker.exe')
    Copy-Item -LiteralPath $dll -Destination $bundle
    New-Item -ItemType Directory -Path (Join-Path $bundle 'third-party-source') | Out-Null
    Copy-Item -LiteralPath (Join-Path $gmp 'gmp.src.tar.zst') -Destination (Join-Path $bundle 'third-party-source/gmp-6.3.0-2.src.tar.zst')
    Copy-Item -LiteralPath (Join-Path $gmp '.BUILDINFO') -Destination (Join-Path $bundle 'third-party-source/gmp-BUILDINFO.txt')
    foreach ($name in @('NotoSansJP.otf','OFL.txt')) {
        Copy-Item -LiteralPath (Join-Path $root "assets/fonts/$name") -Destination (Join-Path $bundle 'assets/fonts')
    }
    Copy-Item -LiteralPath (Join-Path $root 'assets/backgrounds') -Destination (Join-Path $bundle 'assets') -Recurse
    Copy-Item -LiteralPath (Join-Path $root 'assets/audio') -Destination (Join-Path $bundle 'assets') -Recurse
    Copy-Item -LiteralPath (Join-Path $root 'assets/icon') -Destination (Join-Path $bundle 'assets') -Recurse
    New-Item -ItemType Directory -Force -Path (Join-Path $bundle 'assets/rest/journey') | Out-Null
    Copy-Item -LiteralPath (Join-Path $root 'assets/rest/journey/poster.png') -Destination (Join-Path $bundle 'assets/rest/journey')
    Copy-Item -LiteralPath (Join-Path $root 'licenses') -Destination $bundle -Recurse
    foreach ($name in @('LICENSE','README.md','STATUS.md','CONTRIBUTING.md','MAINTAINERS.md','CHANGELOG.md')) {
        Copy-Item -LiteralPath (Join-Path $root $name) -Destination $bundle
    }
    New-Item -ItemType Directory -Path (Join-Path $bundle 'docs') | Out-Null
    foreach ($name in @('guide.md','builtin-world.md','graphics-quality.md','graphics-foundation.md','rest-mode.md','journeys.md','shader-research.md','procedural-landscapes.md','music-licensing.md','assets.md','background-prompts.md','architecture.md','performance.md','design.md','development.md','gmp.md','player.png','rest-gallery.png','rest-water.png','rest-landscape.png','journey.png')) {
        Copy-Item -LiteralPath (Join-Path $root "docs/$name") -Destination (Join-Path $bundle 'docs')
    }
    if ($LocalReview) {
        [IO.File]::WriteAllText((Join-Path $bundle '.stillroom-local-review'), "Local Haskell graphics review; settings stay beside this executable.`n")
        [IO.File]::WriteAllText((Join-Path $bundle 'LOCAL-REVIEW.txt'), "Haskell描画版の試用フォルダーです。公開リリースではありません。`r`nstillroom.exe を開いて試せます。音楽は停止した状態で始まります。`r`n設定・旅の保存場所・取り込んだ画像は、このフォルダーの local-state に保存します。`r`n普段使っているStillroomの設定は書き換えません。終了するときはウィンドウを閉じます。`r`n操作方法は docs/guide.md にあります。`r`n")
    }
    $archiveName = (Split-Path -Leaf $bundle) + '.zip'
    $archive = Join-Path $OutputDirectory $archiveName
    if (Test-Path -LiteralPath $archive) { throw 'Archive already exists.' }
    Compress-Archive -LiteralPath $bundle -DestinationPath $archive -CompressionLevel Optimal
    $hash = (Get-FileHash -LiteralPath $archive -Algorithm SHA256).Hash.ToLowerInvariant()
    [IO.File]::WriteAllText((Join-Path $OutputDirectory 'SHA256SUMS.txt'), "$hash  $archiveName`n")
    Write-Output $archive
} finally { Pop-Location }
