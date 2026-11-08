# Stillroom's Windows dependency adaptations

`tools/prepare-native-dependencies.py` fetches four pinned Hackage source
archives, checks their SHA-256 digests, and prepares local libraries under
`.runtime/gameio-dependencies`. It retains the upstream license in each
prepared package. `tools/build.ps1` runs the preparation before Cabal.

| Upstream | Adaptation |
| --- | --- |
| `hashable-1.5.1.0` | Calls the XXH3 symbols in GHC 9.6.7's RTS directly. Retains seeds, byte ranges and streaming digest behavior. An empty stream update is a no-op, including for a null empty buffer. |
| `primitive-0.9.1.0` | Uses GHC byte-array operations, boot-library byte fills and typed writes. Upstream copy and move operations remain in use. |
| `splitmix-0.1.3.2` | Seeds through Windows `BCryptGenRandom`. The fixed-seed generator remains upstream SplitMix. |
| `text-short-0.1.6.1` | Uses GHC's literal unpackers, boot `text` UTF-8 validation and Haskell byte-array operations. |

To prepare from existing archives without downloading, run:

```powershell
$cache = (cabal path --ignore-project --output-format=json | ConvertFrom-Json).'remote-repo-cache'
python tools/prepare-native-dependencies.py --offline --cache (Join-Path $cache 'hackage.haskell.org')
```

`test/NativeDependencyCheck.hs` can be built against upstream and prepared
package databases with GHC 9.6.7. Each executable writes its compatibility
witness to the path given as its only argument. Compare the two files by hash
or bytewise. It covers hashing, UTF-8, fixed random seeds, numeric fills,
overlap, and offsets. Run Stillroom's `laws` suite for its saved JSON and
other domain behavior.
