# An Idris2 Package Manager with Curated Package Collections

![collection status](https://github.com/stefan-hoeck/idris2-pack/workflows/Check%20DB/badge.svg)

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

For a list of commands and command-line options, type

```sh
pack help
```

In the following sections, we assume the `$PACK_DIR/bin` folder
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

## Directory Structure

It is important to understand, how *pack* keeps track of the
libraries it installed, where it looks for user settings
and package collections, and how it reuses existing
versions of the Idris2 compiler and libraries.

### Package Collections

These are stored as `.toml` files in folder `$HOME/.pack/db`.
If you want to download the latest package collections, you
can do so with the following command:

```sh
pack update-db
```

### Idris Compiler and Libraries

All packages, applications, and different versions of the Idris
compiler can bin found in the subdirectories of
`$HOME./pack/install`. The path to a library or application
includes the commit hash of the Idris compiler it was built with,
as well as the commit hash used for the library or application itself.

For instance, if you installed commit `46bff04` of library
[collie](https://github.com/ohad/collie) after building it
with commit `7a8635` of the Idris compiler, the library will
be found in folder

```sh
$HOME/.pack/install/7a8635/collie/46bff04/
```

The corresponding Idris compiler plus its standard libraries
can be found in directory

```sh
 $HOME/.pack/install/7a8635/idris2
```

Local packages listed in one of your `pack.toml` files will
be installed in a subfolder called `local`. For instance,
I have a local package called `chem`, which - after being
built with the Idris compiler mentioned above - will
be installed in folder

```sh
$HOME/.pack/install/7a8635/local/chem
```

### Application Binaries

These will be installed in subfolder `bin` of the directories
listed above. In addition, a symlink will be added to the
package collection's `bin` folder, which can be found at

```sh
$HOME/.pack/[collection]/bin
```

This will be enough for executing an application via *pack*,
for instance by running

```sh
pack exec katla
```

However, if you want to make these binaries available on your
command line, you need to do two things: First, invoke
`pack` with the `switch` command:

```sh
pack switch nightly-220518
```

And second, add directory `$HOME/.pack/bin` to your `$PATH`
variable.

### Package Path

Since *pack* installs its libraries not in the default locations,
vanilla Idris will not be able to find them (nor will other
applications like
[idris2-lsp](https://github.com/idris-community/idris2-lsp)).
As an additional complication, the locations of libraries
will depend both on the global and local versions of
the `pack.toml` file. It is therefore necessary, to make the
package path available to these tools before you start hacking
on an Idris2 project.

For instance, on my Linux box I use `neovim` with `idris2-lsp` and
the [idris2-nvim](https://github.com/ShinKage/idris2-nvim) plugin.
The `idris2-lsp` application will look for libraries based on
an `.ipkg` file in the project I work on, but these libraries
are scattered all over the place. In order to find them, we
have to add their locations to the `$IDRIS2_PACKAGE_PATH`
environment variable. This can be done as follows:

```sh
IDRIS2_PACKAGE_PATH=$(pack package-path) nvim
```

Likewise, you can invoke the Idris2 compiler in a similar
manner:

```sh
IDRIS2_PACKAGE_PATH=$(pack package-path) idris2
```

If you find these commands to be cumbersome to use, consider
defining an alias for them in the configuration files
of your shell.

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
