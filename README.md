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

```sh
make micropack SCHEME=chez DB=
```

This will set *pack*'s root directory to `$HOME/.pack`
and install the latest nightly package collection. If
you want to use a different package collection, you
can change it with the `$DB` variable. If you want to
use a different directory as *pack*'s root directory,
you can do so by setting the `$PACK_HOME` variable. As an
example, the following will install package collection
`nightly-220430` and use `$HOME/.foo` as *pack*'s root
directory:

```sh
make micropack SCHEME=chez DB=nightly-220430 PACK_DIR="$HOME/.foo"
```

### Installation with an existing Idris2 Compiler

For building *pack* the first time, you will require a recent
installation of Idris2 plus the Idris2 API
(for reading `.ipkg` files). To build, run

```sh
idris2 --build pack.ipkg
```

Afterwards, run the following two commands. If your chez scheme
executable has another name than `scheme` or you want to specify
its full path, you can give it explicitly by using the `-s`
command-line option:

```sh
build/exec/pack update-db
build/exec/pack --bootstrap -s chez switch unstable-220430
```

If run for the first time, this will build and install a recent
version of the Idris2 compiler plus standard libraries and API,
followed by the *pack* application, so this might take a couple of
minutes.

In order to speed things up a bit, you can try and use your existing
Idris2 installation to build the package collection's version of
the Idris2 compiler. In order to do so, remove the `--bootstrap`
command-line option from the last command. For example (if the name
of your *chez scheme* executable is `chez`):

```sh
build/exec/pack -s chez switch unstable-220430
```

### (Optional) Shell Auto-completion

Idris2 supports tab auto-completion for Bash-like shells.

#### For Bash Users

From within bash, run the following command:

```sh
eval "$(pack completion-script pack)"
```

You can also add it to your `.bashrc` file.

#### For ZSH Users

From within ZSH, run the following commands:

```sh
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(pack completion-script pack)"
```

You can also add them to your `.zshrc` file.

## Usage

This assumes the `$PACK_DIR/bin` folder
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
pack remove katla
```

The build tool can run executables, both from local
packages as well as from installed applications.
Use `--args` to pass on command-line arguments (making sure
to properly quote them):

```sh
pack exec test.ipkg -n 50
pack exec katla --help
```

## Customizing Data Collections

User settings go to folder `$PACK_DIR/users`. There, you
can define a custom package collection to be used together
with one of the *official* package collections. For instance,
if you are using `unstable-220430` at the moment, you can
add custom packages to file `$PACK_DIR/users/unstable-220430.db`.

Package collections are still *very* basic. Only two types of
packages are supported at the moment. The first
are GitHub projects, consisting
of a comma-separated name, url, commit hash, and `.ipkg` file.
For instance:

```csv
sop,https://github.com/stefan-hoeck/idris2-sop,af9224510f5c283f3b3c8293524e51c225617658,sop.ipkg
```

The second are local projects, consisting of a comma-separated
name, absolute directory path, and `.ipkg` file. For instance:

```csv
hello,/path/to/hello/project,hello.ipkg
```

In addition, custom packages can also be globally added
to file `$PACK_DIR/users/global.db`. This will be available
with every package collection.

Note, that you can use custom data collections to override
packages listed in an official collection. Packages listed
in `$PACK_DIR/users/global.db` take precedence over those
listed in the official package collection, and custom packages
listed for a specific package collection (for instance, those
in `$PACK_DIR/users/unstable-220430.db`) take precedence
even over global ones.

### Full Example

Assume you'd like to use local libraries `/home/me/hello` and
`/home/me/foo` with all package collections. Here's what
to add to `$PACK_DIR/users/global.db`:

```csv
hello,/home/me/hello,hello.ipkg
foo,/home/me/foo,foo.ipkg
```

In addition, you'd like to override the commit used for the
`katla` project when working with the `unstable-220430`
package collection. Here's what to add to
`$PACK_DIR/users/unstable-220430`:

```csv
katla,https://github.com/idris-community/katla,HEAD,katla.ipkg
```

You could also use the same technique to make *pack* use
a different GitHub repository (probably a fork of yours) for a
certain package.

## Stuff still Missing

There is a lot of functionality still missing. Here's a
non-comprehensive list:

- [x] Support for local package collections
- [ ] Command for starting a REPL
- [ ] Support for custom build directories
- [x] Command for typechecking an Idris package
- [x] Command for building a local Idris2 package
- [x] Command for running an application
- [ ] Command for querying a data collection
- [ ] Command for listing current version of data collection
- [ ] Support for running tests
