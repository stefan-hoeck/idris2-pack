# Contributing to *pack*

Contributions are highly welcome! To help new contributor
find their way around the code base and understand, how
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

TODO
