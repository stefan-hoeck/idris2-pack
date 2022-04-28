module Main

import Pack.Build
import Pack.Types
import Pack.Util

||| Demonstrates the capabilities of *pack* so far.
||| This will install a recent Idris2 executable plus
||| standard libraries and API to folder `$HOME/.pack/<commit-hash>`,
||| before downloading, building, and installing the `katla` executable
||| (plus dependencies), the `hedgehog` library, and the
||| `pack` executable.
|||
||| This is done by consulting a curated package set, which
||| can be found at commit "ba6994813573a744350c2df4d80e1312c6a9babf"
||| of the [idris2-pack-db](https://github.com/stefan-hoeck/idris2-pack-db)
||| repository.
|||
||| This will take several minutes if run for the first time,
||| because the Idris executable will be built and installed.
||| It will be reasonably quick afterwards, unless the
||| commit hash of the executable is manually changed.
covering
main : IO ()
main = run $ do
  e <- env "ba6994813573a744350c2df4d80e1312c6a9babf"
  mkIdris
  installApp "katla"
  installLib "hedgehog"
  installApp "pack"
