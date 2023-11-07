# Developing Libraries and Apps with Pack: Part 1

In this example project, we setup two Idris libraries plus a
test suite, which we plan to develop locally in parallel:

* chem-core-example: Provides some essential data types used
  in cheminformatics. It depends on package *prim*, a library
  available from the official package collection.
* chem-formula-example: Provides a data type for provably
  canonical molecular formulae. This depends on *chem-core-example*,
  which is *not* part of the official package collection.
* chem-formula-example-test: A test suite for the
  chem-formula-example library, mainly used to run a
  round-trip test for its molecular formula parser.
  This depends on *hedgehog* (plus some transitive dependencies)
  for running the property tests, plus of course
  *chem-formula-example* and *chem-core-example*, both of which
  are again not part of the official package collection.

In order to make the custom libraries visible to pack, we need
to add a `pack.toml` file to the project's root directory. Look
at the comments in that file for some additional notes on
relative and absolute paths. This lists the two libraries plus the
test suit as local projects.

Pack will look for `pack.toml` files in the current working
directory plus all its parent directories and will stop at the
first one it finds. This allows us to make use of these files
even when we move into one of the subfolders of a project.
Settings listed in local `pack.toml` files
take precedence over the ones given in the global `pack.toml` file
at `$PACK_DIR/user/pack.toml` (remember that `$PACK_DIR` defaults
to `$HOME/.pack`).

## Type-checking, Building, and Testing with pack

Type-checking an Idris package with pack is straightforward:

```sh
pack typecheck chem-formula/chem-formula-example.ipkg
```

Note, how pack will automatically build and install all the
necessary dependencies (including the local ones). Likewise,
we can build a project:

```sh
pack build chem-formula/test/chem-formula-example-test.ipkg
```

In case of applications, it is also possible to build and run
them directly. For instance, the following runs the test suite
for molecular formulae:

```sh
pack run chem-formula-example-test
```

We can also pass additional command-line arguments to an
executable:

```sh
pack run chem-formula-example-test -n 10000
```

Finally, we can also use pack to start REPL sessions. If
we load a specific Idris source file, pack will look for an
`.ipkg` file in the file's parent directories and will
automatically install the required dependencies first.
Let's try this by first removing package *chem-core-example*:

```sh
pack remove chem-core-example
```

Now, let's start a REPL session to try some stuff on our molecular
formula data type:

```sh
pack repl chem-formula/src/Chem/Formula.idr
```

Note, how pack will first re-install *chem-core-example*. It will
do the same thing in case of outdated dependencies (see next section).

## Outdated Packages

When we work on several local libraries in parallel, pack keeps
track of our changes and will reinstall any outdated dependencies
automatically. For instance, if you have *chem-core-example* already installed
and update the timestamp of one of its source files, pack will
list the package as being "outdated" when you run `pack info`.
If you try to build *chem-formula-example* or its test suite again,
the outdated library will first be rebuilt and installed. All
this happens without further ado from our part.

## Developing with External Tools

Many Idris developers make use of tools like
[idris2-lsp](https://github.com/idris-community/idris2-lsp)
or the Idris ide-mode when coding in Idris. While pack can
make its managed packages available to these tools (for instance,
if you use *idris2-lsp*, you can conveniently install it via
pack and it will automatically have access to all libraries
installed by pack: `pack install-app idris2-lsp`), these tools will typically
not install or update any required dependencies automatically.
In this case it is necessary to first run `pack install-deps my.ipkg`
before you start coding on a project.
