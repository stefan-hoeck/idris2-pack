# Contributing to *pack*

Contributions are highly welcome! To help new contributor
find their way around the codebase and understand, how
*pack* is implemented, this document provides a source
map and some details about the structure of the `$HOME/.pack`
directory.

## Directory Layout

The root directory *pack* works on is located at `$HOME/.pack`,
although this can be changed by setting environment variable
`$PACK_DIR`.

### Data Collections

These are stored in folder `$HOME/.pack/db` and have a `.db`
file ending. At the moment, they are just comma separated
lists of fields in the following order:

```db
package-name,url,commit-hash,ipkg-file
```

The header of a `.db` consists of two comma separated fields:
The commit hash of the Idris2 version to use followed
by the Idris2 version as given in in the `idris2.ipkg` file.

### User Settings

These are stored in `$HOME/.pack/user`. At the moment, there
are two kinds of files recognized by *pack*: `global.db` lists
user defined packages as described in the README, which will
be included with every package collection, and `[collection].db`
lists user defined packages, which will only be included when
working with `[collection]`.

### Patches

The package manager supports patching (of `.ipkg` files only, so
far). Patches must be generated with

```sh
patch original.ipkg patched.ipkg
```

They are then stored in file

```sh
$HOME/.pack/patches/[collection name]/[package name]/[name].ipkg.patch
```

### Cache

*pack* does not provide `.ipkg` files itself (that would be
a duplication of information, which could easily get out of
sync with upstream), so during package resolution it will have
to fetch these files from GitHub. Since this can be time
consuming, `.ipkg` files will be cached once downloaded
at the following location:

```sh
$HOME/.pack/.cache/[package name]/[commit hash]/[name].ipkg
```

### Installed Binaries and Libraries

*pack* installs the Idris2 compiler and libraries in
directory `$HOME/.pack/[collection name]`. With
`pack switch [collection name]`, two sym links are created:

`$HOME/.pack/bin` pointing at `$HOME/.pack/[collection name]/bin`,
and `$HOME/.pack/idris2`, pointing at `$HOME/.pack/[collection name]`.

Command `pack switch [collection name]` also stores the name of
the package collection in file `$HOME/.pack/.db`, which will
serve as the default collection used by *pack*.

## Map of the Source Code

The sections below give an overview of the project's source
code and where to look for certain pieces of functionality.

### Core

Submodules of `Pack.Core` implement the application's core
functionality.

#### Types

Most data types can be found in `Pack.Core.Types`.
Since we work a lot with strings having distinct semantics,
most string types are wrapped in single-field records to
dramatically increase type safety. Contributors are expected
to do the same when introducing new use cases for strings.

We extract a lot of information from upstream `.ipkg` files.
Module `Pack.Core.Ipkg` provides functionality for parsing
these files. A lot of this was copied from the Idris2 sources
and just slightly adjusted to our needs, so this should
probably go back to Idris2 eventually.

#### IO

Most *pack* subprograms run in `EitherT PackErr IO`, where
`PackErr` is an error type defined in `Pack.Core.Types`.
Module `Pack.Core.IO` provides utility functions for working
with our IO type, and for handling files and folders as wells
as system calls.

*git* related functionality such as cloning GitHub projects
and checking out to specific commits is provided in module
`Pack.Core.Git`.

### Package Collections

Types for working with package collections are defined
in `Pack.Database.Types`. Our data collections only store
a minimal amount of information, represented by
`Pack.Database.Types.Package`.

A whole package collection consists of the Idris2 commit
to use, the corresponding version of Idris2, and a
`SortedMap` of packages. All this is collected in record
`Pack.Database.Types.DB`.

Once users show interest in a certain package, this typically
has to be *resolved*, either by downloading its `.ipkg` file from
GitHub, or by reading the same file from the local cache.
The result is stored in a
`Pack.Database.Types.ResolvedPackage`.

### Command Line

Subfolder of `CmdLn` provide functionality for handling
command line arguments and assembling a configuration
value before doing the interesting stuff.

#### Program Configuration

Most pack actions take a value of type
`Pack.CmdLn.Types.Config` as an argument. This type is
indexed over a state type, signalling what kind of
initialisation actions have already been performed:

  * `Nothing` : Just the core configuration from command line
     options and some settings in the `$HOME/.pack` directory
     have been assembled.

  * `Just DBLoaded` : Core configuration has been assembled
    and the requested package collection has been loaded into
    memory. This is typically sufficient for querying package
    collections, but not for using Idris2 for building
    packages.

  * `Just HasIdris` : Like `Just DBLoaded`, but in addition,
    *pack* verified that the selected package collections already
    has a local installation of the Idris compiler. If this is
    not the case, the compiler will be built and installed.

#### Command Line Arguments and Tab Completion

Parsing of command line arguments is handled in module
`Pack.CmdLn.Opts`. Assembling of `Config` values from
different resources is done by the functions in
module `Pack.CmdLn.Env`.

Finally, tab completion is implemented in `Pack.CmdLn.Completion`.

### Running Commands

The available commands (such as `install-app` or `typecheck`)
are defined in data type `Pack.CmdLn.Types.Cmd`, but
command handlers are implemented in the submodules of
`Pack.Runner`.

Module `Pack.Runner.Database` provides functionality for
querying the installed libraries and executables, but
its most important piece of functionality is function
`resolve`, which is used for converting a package
(represented as `Pack.Database.PkgRep`, which is either
just a path to an `.ipkg` file in the local file system,
or the name of a package managed by *pack* in a
package collection) to a `ResolvedPackage`, which comes
with a parsed `.ipkg` file of type
`Idris.Package.Types.PkgDesc`.

Module `Pack.Runner.Check` provides functionality for
running sanity checks on a package collection and printing
a report with the results.

Module `Pack.Runner.Install` implements actions for
installing libraries and applications, as well as
building and type checking local projects.

Finally, module `Core.Runner` provides function `runCmd`,
which is entry point of the application (modulo error handling).
