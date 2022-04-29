# An Idris2 Package Manager with a Curated Package Collection

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

The *pack* application will install all libraries and executables
to directory `$PACK_DIR`, which defaults to `$HOME/.pack`.
It's therefore recommended to add the `$PACK_DIR/bin` folder
to your `$PATH`.

For building *pack* the first time, you will require a recent
installation of Idris2 plus the Idris2 API
(for reading `.ipkg` files). To build, run

```sh
idris2 --build pack.ipkg
```

Afterwards, run

```sh
build/exec/pack --switch unstable-220429
```

If run for the first time, this will build and install a recent
version of the Idris2 compiler plus standard libraries and API,
followed by the *pack* application, so this might take a couple of
minutes.

## Usage

This assumes `$PACK_DIR/bin` is on your path and you have installed
*pack* as described above. To install a library from the 
package collection, run

```sh
pack --install hedgeog
```

This will download and build the
[idris2-hedgehog](https://github.com/stefan-hoeck/idris2-hedgehog)
library together with all its dependencies.

To build and install an application (for instance, the
[katla](https://github.com/idris-community/katla) app),
run

```sh
pack --install-app katla
```

It is also possible to work with local `.ipkg` files as long
as they depend on packages known to *pack*:

```sh
pack --install-app fix_whitespace.ipkg
```

## Stuff still Missing

There is a lot of functionality still missing. Here's a
non-comprehensive list:

[ ] Support for local package collections
[ ] Command for starting a REPL
[ ] Support for custom build directories
[ ] Command for typechecking an Idris package
[ ] Command for querying a data collection
[ ] Command for listing current version of data collection
[ ] Support for running tests
