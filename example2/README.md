# Developing Libraries and Apps with Pack: Part 2

In this second example project, I show how to use pack to
work on several packages in parallel via GitHub. In this case it
is assumed that several people collaborate on these packages,
pushing their changes to GitHub on regular occasions. It is
cumbersome for programmers depending on these libraries to manually
check for updates and pull the latest commits. We'd rather let
pack handle the boring stuff.

Here's the project structure:

* chem-smiles-example: A simplified SMILES parser for molecules.
  In addition to some officially supported dependencies, this also
  depends on *chem-core-example* and *chem-formula-example* from the
  first example project, but we assume these are maintained by
  different programmers in our team, while we are responsible of
  implementing the SMILES parser.
* chem-smiles-example-test: A small test suite for the SMILES parser.

As in the first example, we add a `pack.toml` file to the project's
root directory listing all custom libraries and apps including the
one we are developing at the moment plus its test suite. Note, how
we specify the *chem-core-example* dependency:

```toml
[custom.all.chem-core-example]
type   = "github"
url    = "https://github.com/stefan-hoeck/idris2-pack"
commit = "latest:main"
ipkg   = "example1/chem-core/chem-core-example.ipkg"
```

This is a GitHub dependency, with a so-called *meta commit*:
A commit that first needs to be resolved by pack. It tells pack
to use the latest commit from the *main* branch of the *idris2-pack*
GitHub repository. However, connecting to GitHub and fetching
the latest commit hash can be slow and lead to increased
startup times for pack. Pack will therefore only fetch the
commit hash if it hasn't done so before, or if it is being told
to do so explicitly by invoking `pack fetch`.

The *chem-formula-example* dependency is almost identical, but
this time we want to make sure we never miss any updates:
The `fetch-latest:main` meta commit tells pack to fetch
the latest commit hash from the main branch on every startup.
As noted in the `pack.toml` comments: Only use this if you truly need
it as it can significantly slow down pack:

```toml
[custom.all.chem-formula-example]
type   = "github"
url    = "https://github.com/stefan-hoeck/idris2-pack"
commit = "fetch-latest:main"
ipkg   = "example1/chem-formula/chem-formula-example.ipkg"
```
