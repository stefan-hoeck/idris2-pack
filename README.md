# An Idris2 Package Manager with Curated Package Collections

[![Check Collection](https://github.com/stefan-hoeck/idris2-pack-db/actions/workflows/ci-db.yml/badge.svg)](https://github.com/stefan-hoeck/idris2-pack-db/blob/main/STATUS.md)

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
available to pack. The list of currently available packages plus
their current build status can also be found
[here](https://github.com/stefan-hoeck/idris2-pack-db/blob/main/STATUS.md).

## Quick Installation

For detailed instructions and prerequisites, see [installation](INSTALL.md).
Assuming, you have already installed Chez Scheme
you can set up pack and the corresponding Idris2
compiler with the following command:

```sh
bash -c "$(curl -fsSL https://raw.githubusercontent.com/stefan-hoeck/idris2-pack/main/install.bash)"
```

You will be asked about the name of your Chez Scheme executable during
the installation procedure. If all goes well, make sure to add
folder `$HOME/.pack/bin` to your `$PATH` variable.

## Usage

For a list of commands and command-line options, type

```sh
pack help
```

In the following sections, we assume the `$PACK_DIR/bin` folder
is on your path and you have installed
pack as described under [installation](INSTALL.md).

To create a new library project, type

```sh
pack new lib idris2-library
```
replacing `idris2-library` with the name of your library.
This will create a new package in the current directory consisting of a source directory, a default module, a skeleton test suite, a local pack.toml file and a .ipkg file.
A git repository will also be initialized together with a suitable `.gitignore` file.
If you wish to create a new application project, replace `lib` with `app`.

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

> **Note**
>
> Idris packages can contain additional instructions to run before and after build and installation of a package,
> we call them *custom build hooks*.
> This can be potentially dangerous because hooks may invoke arbitrary code in your system.
> By default pack prompts for continuation in case when requested package contains them, e.g.
>
> ```sh
> Package lsp uses custom build hooks. Continue (yes/*no)?
> ```

If you no longer require *katla* and want to remove it, run

```sh
pack remove katla
```

It is also possible to work with local `.ipkg` files as long
as they depend on packages known to pack:

```sh
pack build json.ipkg
pack typecheck elab-util.ipkg
```

The build tool can run executables, both from local
`.ipkg` files as well as from installed applications. Command
line arguments to be passed on to the executable can be
listed after the package name or `.ipkg` file:

```sh
pack run test.ipkg -n 50
pack run katla --help
```

You can use pack to start an Idris REPL session, optionally
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
to the set of packages known to pack. For instance, assuming you
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

Likewise, you could at a GitHub project not yet known to pack
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
precedence over global once. Pack will look for local `pack.toml`
files in all parent directories of the current working directory
(including the current working directory itself) and will stop
at the first one it finds.

## Directory Structure

It is important to understand, how pack keeps track of the
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
compiler can be found in the subdirectories of
`$HOME/.pack/install`. The path to a library or application
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
listed above. In addition, a wrapper script will be added to the
package collection's `bin` folder, which can be found at

```sh
$HOME/.pack/[collection]/bin
```

This will be enough for executing an application via pack,
for instance by running

```sh
pack exec katla
```

However, if you want to make these binaries available on your
command-line, you need to do two things: First, invoke
`pack` with the `switch` command:

```sh
pack switch nightly-220518
```

And second, add directory `$HOME/.pack/bin` to your `$PATH`
variable.

## Developing Applications

There are two example projects explaining in detail how to use
pack to develop groups of related projects. The first, which
can be found in the `example1` subfolder, sets up
two libraries plus a test suite for local development.
A detailed description how it works can be found [here](example1/README.md).

The second example in the folder `example2` explains how to collaborate
on several packages in parallel via git. Details can be found
[here](example2/README.md).

> **Note**
>
> Please notice that if your application relies on the `IDRIS2_PACKAGE_PATH`
> environment variable (for example, an alternative backend for Idris) or your
> package uses a shared library or support files, set `packagePath` option to
> `true` in an appropriate section for you package in a `pack.toml` (both local
> and in the pack collection).
> You can see an example of such usage [here](https://github.com/stefan-hoeck/idris2-pack-db/blob/bcc8dc61706c73361bb1e6e18dd1b0c5981f0e18/collections/HEAD.toml#L297).
> Technical details can be found [here](https://github.com/stefan-hoeck/idris2-pack/issues/256#issuecomment-1689305587).

## Uninstallation

If you would like to uninstall pack from your system, you can simply use the following command:

```sh
pack uninstall
```

This will delete the `$PACK_DIR` directory.
