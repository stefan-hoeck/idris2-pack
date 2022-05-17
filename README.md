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

## Quick Installation

For detailed instructions and prerequisites, see [installation](INSTALL.md).
Assuming, you have already installed Chez Scheme (and its
executable is called `chez`) and you want to start with
the `nightly-220507` package collection, you can set up
*pack* and the corresponding Idris2 compiler with the following
command:

```sh
make micropack SCHEME=chez DB=nightly-220507
```

## Usage

This assumes the `$PACK_DIR/bin` folder
is on your path and you have installed
*pack* as described under [installation](INSTALL.md).
To install a library from the package collection, run

```sh
pack install hedgehog
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

If you no longer require *katla* and want to remove it, run

```sh
pack remove katla
```

It is also possible to work with local `.ipkg` files as long
as they depend on packages known to *pack*:

```sh
pack install-app fix_whitespace.ipkg
pack build json.ipkg
pack typecheck elab-util.ipkg
```

The build tool can run executables, both from local
packages as well as from installed applications. Command
line arguments to be passed on to the executable can be
listed after the package name or `.ipkg` file:

```sh
pack exec test.ipkg -n 50
pack exec katla --help
```

You can use *pack* to start an Idris REPL session, optionally
with making dependencies listed in an `.ipkg` file available
(these will first be built and installed if necessary):

```sh
pack repl
pack repl Test.idr
pack --with-ipkg rhone.ipkg repl src/Data/MSF.idr
```

## Customization

User settings are stored in file `$PACK_DIR/user/pack.toml`.
This file should have been generated automatically by pack
when setting up the application for the first time. The
different settings have been annotated with comments to
make it more accessible.

If you want to start using a new package collection,
edit the `collection` field accordingly:

```toml
collection = "nightly-220507"
```

It is also possible to add local projects as well as GitHub
projects not yet managed by your package collection of choice
to the set of packages known to *pack*. For instance, assuming you
have a local project called `hello` located in directory
`/data/me/idris/hello` with `.ipkg` file `hello.ipkg`,
and you want to make this available to all package collections
you work with, add the following lines to `pack.toml`:

```toml
[custom.all.hello]
type = "local"
path = "/data/me/idris/hello"
ipkg = "hello.ipkg"
```

If, on the other hand, you want to make this package only available
to package collection `nightly-220506`, change the above to the
following:

```toml
[custom.nightly-220506.hello]
type = "local"
path = "/data/me/idris/hello"
ipkg = "hello.ipkg"
```

Likewise, you could at a GitHub project not yet known to *pack*
to one or all of the package collections:

```toml
[custom.nightly-220506.hashmap]
type   = "github"
url    = "https://github.com/Z-snails/idris2-hashmap"
commit = "cb97afaa7c5d79dcb85901c6f5f87bed906fed81"
ipkg   = "hashmap.ipkg"
```

Custom packages take precedence over official ones, so it is
possible to override an officially supported package with
a custom version of yours (either a local clone or perhaps
a fork on GitHub).

### Local `pack.toml` Files

You can also add a `pack.toml` file locally to the root folder
of a project. Just as with the global `pack.toml` file in directory
`$HOME/.pack/user/`, you can specify the package collection to
use for a project as well as define additional local dependencies
and even override global package settings. Local settings take
precedence over global once.

A note of caution: At the moment, *pack* uses a single installation
directory (located at `$HOME/.pack/collection-name/idris2-[idris-version]`)
for all dependencies. If you override global dependencies with local
ones, it may be hard to figure out, which version of a library is
installed at the moment. I'll have to think carefully about how
to resolve this, because this is a serious limitation.

## Stuff still Missing

There is a lot of functionality still missing. Here's a
non-comprehensive list:

- [x] Support for local package collections
- [x] Command for starting a REPL session
- [ ] Support for custom build directories
- [x] Command for typechecking an Idris package
- [x] Command for building a local Idris2 package
- [x] Command for running an application
- [x] Command for querying a data collection
- [ ] Command for listing current version of data collection
- [ ] Support for running tests
