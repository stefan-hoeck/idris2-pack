# An Idris2 Package Manager with Curated Package Collections

This is a simple package manager taking a slightly different
approach than other available options like
[sirdi](https://github.com/eayus/sirdi) or
[inigo](https://github.com/idris-community/inigo): It makes use
of curated collections of packages linked to a specific version/commit
of Idris2, which are guaranteed to properly work together (otherwise,
that's a bug in the package collection). This is similar to what
*stack* for Haskell does: It avoids dependency hell by design.

There is a second GitHub repository containing the package collections:
[idris2-pack-db](https://github.com/stefan-hoeck/idris2-pack-db).
See instructions there if you want to make your own packages
available to *pack*.

## Installation

Pack will operate solely on the *pack root directory*,
which defaults to `$HOME/.pack` and can be changed by
setting environment variable `$PACK_DIR`.

In order to make use of the binaries installed by *pack*,
make sure that folder `bin` of *pack*'s root directory is
on your path. In order for the Idris2 compiler managed by
*pack* to take precedence over the one you might already
have installed, this folder should appear *before* the
`bin` folder where your other Idris2 installation resides.
For instance:

```sh
export PATH="$HOME/.pack/bin:$HOME/.idris2/bin:..."
```

For building *pack* the first time, you will require a recent
installation of Idris2 plus the Idris2 API
(for reading `.ipkg` files). To build, run

```sh
idris2 --build pack.ipkg
```

Afterwards, run the following two commands. If your chez scheme
executable has another name than `scheme` or you want to specify
its full path, you can give it explicitly by using the `-s`
command line option:

```sh
build/exec/pack update-db
build/exec/pack -s chez switch unstable-220430
```

If run for the first time, this will build and install a recent
version of the Idris2 compiler plus standard libraries and API,
followed by the *pack* application, so this might take a couple of
minutes.

## Usage

This assumes the `bin` folder of *pack*'s root directory
is on your path and you have installed
*pack* as described above. To install a library from the 
package collection, run

```sh
pack install hedgeog
```

This will download and build the
[idris2-hedgehog](https://github.com/stefan-hoeck/idris2-hedgehog)
library together with all its dependencies.

To build and install an application (for instance, the
[katla](https://github.com/idris-community/katla) app),
run

```sh
pack install-app katla
```

It is also possible to work with local `.ipkg` files as long
as they depend on packages known to *pack*:

```sh
pack install-app fix_whitespace.ipkg
pack build json.ipkg
pack typecheck elab-util.ipkg
```

The build tool can run executables, both from local
packages as well as from installed applications.
Use `--args` to pass on command line arguments (making sure
to properly quote them):

```sh
pack exec test.ipkg -n 50
pack exec katla --help
```

## Stuff still Missing

There is a lot of functionality still missing. Here's a
non-comprehensive list:

- [ ] Support for local package collections
- [ ] Command for starting a REPL
- [ ] Support for custom build directories
- [x] Command for typechecking an Idris package
- [x] Command for building a local Idris2 package
- [x] Command for running an application
- [ ] Command for querying a data collection
- [ ] Command for listing current version of data collection
- [ ] Support for running tests
