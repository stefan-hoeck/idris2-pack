# idris2-pack

This requires a recent installation of Idris2 plus the Idris2 API
(for reading `.ipkg` files). To build, run

```sh
idris2 --build pack.ipkg
```

Afterwards, run

```sh
build/exec/pack
```

Running the application will take several minutes the first time,
because a fresh version of Idris2 is installed to directory
`$HOME/.pack/<commit-hash>`. Afterwards, the `katla` executable
and its dependencies are built and installed, the `hedgehog`
package plus dependencies is installed, and the `pack` application
itself is installed.

At the moment, the philosophy of
`pack` is to use a curated package set (which is
hard-coded to `Pack.Types.db`) together with a
specific Idris2 commit, used for building the packages.
The packages are supposed to be tested to be compatible with each other.

The idea is to have a package manager similar to Haskell's *stack*,
where packages are being built and developed against a
curated package set. This should not only make it easy to have
several versions/commits of the same package installed, but also
to have several versions/commits of Idris2 installed and to
easily switch between these versions (switching not yet implemented).
