# GMP in Windows releases

Stillroom source remains MIT licensed. The release also uses GNU MP (GMP) 6.3.0
under **LGPL version 3 or later**, separately from the application license.

`tools/build.ps1` obtains the pinned MSYS2 UCRT64 package `6.3.0-2` and its
corresponding source package from the official mirror. SHA-256 checks cover both.
The complete source package, patches and PKGBUILD are included in the release
under `third-party-source/gmp-6.3.0-2.src.tar.zst`.

GHC's Windows archive normally contains static GMP. The build script links the
GMP DLL import library before those objects, retaining import entries with
`--whole-archive`. The resulting application uses `libgmp-10.dll` beside the EXE.
The global GHC installation is not modified.

You may modify or replace this DLL with an interface-compatible version and may
reverse engineer the combined application to debug those modifications. Stillroom
does not impose a signature check or restriction on replacement. Keep the UCRT64
ABI, 64-bit limbs and exported interface (including the package's `--enable-fat`
build) compatible. To rebuild the library, extract the source package in an MSYS2
UCRT64 development environment and run `makepkg-mingw` using its supplied
PKGBUILD. It includes upstream source, patches and configuration flags. Install
the resulting DLL beside the application; no application-source changes are
required for a compatible replacement.

The release includes both `licenses/GMP-LGPL-3.txt` and `licenses/GMP-GPL-3.txt`.
See the [official MSYS2 package](https://packages.msys2.org/packages/mingw-w64-ucrt-x86_64-gmp)
and [GNU LGPL](https://www.gnu.org/licenses/lgpl-3.0.html). An ordinary
`cabal build` without the build script may statically link GMP and is not the
binary distribution procedure used here.
