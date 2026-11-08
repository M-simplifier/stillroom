"""Prepare the pinned Windows x64 Haskell dependency adaptations for Stillroom."""

import argparse
import hashlib
import io
from pathlib import Path, PurePosixPath
import re
import tarfile
from tempfile import NamedTemporaryFile
from urllib.request import urlopen


ROOT = Path(__file__).resolve().parents[1]
OVERLAY = ROOT / "tools" / "native-dependencies"
OUTPUT = ROOT / ".runtime" / "gameio-dependencies"
PACKAGES = {
    "hashable": ("1.5.1.0", "ed0b7c10ce92c9ffe45420dadc38c6d39db486ff3633ff13567cc9f75f79b112"),
    "primitive": ("0.9.1.0", "44b4de41813c7bc5db8a57f87c3612a069b65086946268ba165097252ebd3d76"),
    "splitmix": ("0.1.3.2", "a61d4e8b30f5a16526d7d31171b674ae7924d2207f378060d13363bd8794de8c"),
    "text-short": ("0.1.6.1", "83669f402354274fbcdc75126443af733267b955e451f72c0c6d86ae8da87416"),
}


def read_sources(name, version, digest, cache, offline):
    stem = f"{name}-{version}"
    archive = OUTPUT / "archives" / f"{stem}.tar.gz"
    candidates = [archive]
    if cache is not None:
        candidates.insert(0, cache / name / version / archive.name)
    cached = next((path for path in candidates if path.is_file()), None)
    if cached is not None:
        data = cached.read_bytes()
    elif offline:
        raise FileNotFoundError(f"Missing cached dependency archive: {stem}")
    else:
        url = f"https://hackage-content.haskell.org/package/{stem}/{stem}.tar.gz"
        with urlopen(url, timeout=30) as response:
            data = response.read(8 * 1024 * 1024)
    if hashlib.sha256(data).hexdigest() != digest:
        raise ValueError(f"{stem} source archive SHA-256 differs")
    if cached is None:
        write_if_changed(archive, data)
    files = {}
    with tarfile.open(fileobj=io.BytesIO(data), mode="r:gz") as source:
        for entry in source:
            path = PurePosixPath(entry.name)
            if not entry.isfile() or path.parts[0] != stem or ".." in path.parts:
                continue
            relative = PurePosixPath(*path.parts[1:])
            if relative.name.upper() == "LICENSE" or relative.suffix == ".cabal" or (
                relative.suffix in (".hs", ".hs-boot") and relative.parts[0] in ("src", "src-ghc804", "Control", "Data")
            ):
                with source.extractfile(entry) as handle:
                    files[str(relative)] = handle.read()
    return files


def write_if_changed(path, data):
    if not path.is_file() or path.read_bytes() != data:
        path.parent.mkdir(parents=True, exist_ok=True)
        handle = NamedTemporaryFile(dir=path.parent, delete=False)
        temporary = Path(handle.name)
        try:
            with handle:
                handle.write(data)
            temporary.replace(path)
        finally:
            temporary.unlink(missing_ok=True)


def replace_once(text, old, new):
    if text.count(old) != 1:
        raise ValueError(f"Pinned source anchor differs: {old!r}")
    return text.replace(old, new)


def replace_between(text, start, end, replacement):
    left = text.index(start)
    right = text.index(end, left + len(start))
    return text[:left] + replacement + text[right:]


def library_manifest(text):
    text = re.split(r"(?m)^(?:test-suite|benchmark|executable)\s", text, maxsplit=1)[0]
    return re.sub(r"(?im)^extra-(?:source|doc)-files:[^\n]*\n(?:[ \t]+[^\n]*\n)*", "", text)


def adapt_hashable(files, manifest):
    manifest = replace_between(manifest, "  include-dirs:", "  hs-source-dirs:", "")
    manifest = replace_between(manifest, "  if (flag(random-initial-seed)", "  default-language:", "")
    manifest = replace_between(manifest, "  if flag(arch-native)", "  if impl(ghc >=9.0)", "")
    manifest = replace_once(manifest, "    Data.Hashable.FFI\n", "")
    files.pop("src/Data/Hashable/FFI.hs")
    files["src/Data/Hashable/XXH3.hs"] = (OVERLAY / "HashXXH3.hs").read_bytes()
    return manifest


def adapt_splitmix(files, manifest):
    manifest = replace_between(manifest, "  if impl(ghcjs)", "source-repository head", "  extra-libraries: bcrypt\n\n")
    files["src/System/Random/SplitMix/Init.hs"] = (OVERLAY / "SplitMixSeed.hs").read_bytes()
    return manifest


def adapt_primitive(files, manifest):
    manifest = manifest[:manifest.index("  Include-Dirs: cbits")] + "\n"
    types = "Data/Primitive/Types.hs"
    source = files[types].decode()
    source = replace_once(source, "import Data.Primitive.Internal.Operations\n", "import qualified Foreign.Marshal.Utils as Memory\n")
    source = replace_once(source, "import GHC.IO (IO(..))\n", "")
    source = replace_between(source, "; setByteArray# arr# i# n# (ctr x#) s#", "; indexOffAddr#", "; setByteArray# arr# i# n# x s# = set_arr arr# i# n# x s# \\\n")
    source = replace_between(source, "; setOffAddr# addr# i# n# (ctr x#) s#", "; {-# INLINE sizeOfType#", "; setOffAddr# addr# i# n# x s# = set_addr addr# i# n# x s# \\\n")
    source = replace_between(source, "#if __GLASGOW_HASKELL__ >= 902\nliberate#", "unI# ::", (OVERLAY / "PrimitiveFill.inc").read_text(encoding="utf-8") + "\n")
    for scalar in ("Word", "Int", "Word16", "Word32", "Word64", "Int16", "Int32", "Int64", "Float", "Double"):
        source = source.replace(f"set{scalar}Array#", "fillNumericArray#").replace(f"set{scalar}OffAddr#", "fillNumericAddress#")
    for scalar in ("Int8", "Word8"):
        source = replace_once(source, f"shimmedSet{scalar}Array#", "fillOctetArray#")
        source = replace_once(source, f"set{scalar}OffAddr#", "fillOctetAddress#")
    for scalar in ("Addr", "StablePtr", "WideChar"):
        source = source.replace(f"set{scalar}Array#", "defaultSetByteArray#").replace(f"set{scalar}OffAddr#", "defaultSetOffAddr#")
    files[types] = source.encode()
    operations = "Data/Primitive/Internal/Operations.hs"
    source = files[operations].decode()
    source = replace_between(source, "  setWord8Array#", "#if defined(HAVE_KEEPALIVE)\n  , keepAlive", "  UnliftedType\n")
    source = replace_once(source, "  , UnliftedType\n", "")
    source = replace_once(source, "import Data.Primitive.MachDeps (Word64_#, Int64_#)\n", "")
    source = replace_once(source, "import Foreign.C.Types\n", "")
    source = replace_between(source, "#if __GLASGOW_HASKELL__ >= 902\nforeign import", "#if defined(HAVE_KEEPALIVE)\nkeepAliveLiftedLifted#", "")
    files[operations] = source.encode()
    return manifest


def adapt_text_short(files, manifest):
    manifest = replace_once(manifest, "  other-modules:    Data.Text.Short.Internal\n", "  other-modules:    Data.Text.Short.Internal, Data.Text.Short.Native\n")
    manifest = replace_once(manifest, "  c-sources:        cbits/cbits.c\n", "")
    manifest = replace_once(manifest, "  else\n    cc-options: -DNDEBUG=1\n", "")
    manifest = replace_once(manifest, "  cc-options:       -Wall\n", "")
    internal = "src/Data/Text/Short/Internal.hs"
    source = files[internal].decode()
    source = replace_once(source, "import qualified Data.Text                      as T\n", "import qualified Data.Text                      as T\nimport           Data.Text.Short.Native\n")
    source, removed = re.subn(r'(?m)^foreign import ccall unsafe "hs_text_short_(?!mutf8_)[^\n]+\n', "", source)
    if removed != 9:
        raise ValueError(f"Expected nine text-short primitive imports, got {removed}")
    source = replace_between(source, "{-# NOINLINE fromLitAsciiAddr# #-}", "-- $setup", """{-# NOINLINE fromLitAsciiAddr# #-}
fromLitAsciiAddr# :: Addr# -> ShortText
fromLitAsciiAddr# address# = fromString (GHC.unpackCString# address#)

{-# NOINLINE fromLitMUtf8Addr# #-}
fromLitMUtf8Addr# :: Addr# -> ShortText
fromLitMUtf8Addr# address# = fromString (GHC.unpackCStringUtf8# address#)

""")
    source = replace_once(source, "import           Control.Monad.ST               (stToIO)\n", "")
    source = replace_once(source, "                                                 Ptr (..), RealWorld, Word (W#))", "                                                 Word (W#))")
    source = replace_once(source, "type CCodePoint = Word\n", "")
    source = replace_once(source, "data MBA s = MBA# { unMBA# :: MutableByteArray# s }", "data MBA s = MBA# (MutableByteArray# s)")
    source = replace_between(source, "{-# INLINE copyAddrToByteArray #-}", "----------------------------------------------------------------------------\n-- unsafeFreezeShrink", "")
    files[internal] = source.encode()
    files["src/Data/Text/Short/Native.hs"] = (OVERLAY / "TextShortPrimitives.hs").read_bytes()
    return manifest


ADAPT = {
    "hashable": adapt_hashable,
    "primitive": adapt_primitive,
    "splitmix": adapt_splitmix,
    "text-short": adapt_text_short,
}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cache", type=Path, help="Use Hackage's existing local archive tree")
    parser.add_argument("--offline", action="store_true", help="Require the four archives to be cached")
    args = parser.parse_args()
    for name, (version, digest) in PACKAGES.items():
        files = read_sources(name, version, digest, args.cache, args.offline)
        cabal = f"{name}.cabal"
        manifest = ADAPT[name](files, library_manifest(files[cabal].decode()))
        files[cabal] = (manifest.rstrip() + "\n").encode()
        target = OUTPUT / f"{name}-{version}"
        for relative, data in files.items():
            write_if_changed(target / relative, data)
        print(f"Prepared {target}")


if __name__ == "__main__":
    main()
