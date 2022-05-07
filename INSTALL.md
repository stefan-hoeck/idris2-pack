# Installing

This section describes in detail the installation
procedure for installing *pack* for the first time.
Although it is possible to use an existing Idris2
compiler to build and install pack, it is best to use
the *micropack* application bundled with this project.

## 1. Preparations

The following libraries and applications are required
for working with Idris2. Make sure you have them installed
before you continue:

* A Scheme compiler; either Chez Scheme (default), or Racket.
  If you install Chez Scheme from source files, building it locally,
  make sure you run ./configure --threads to build multithreading support in.

  Note: Racket support has not yet been built into *pack*. This
  is an open issue.

* `bash`, `GNU make`, `sha256sum`, and `GMP`. On Linux, you probably
  already have these. On macOS and major BSD flavours, you can install them
  using a package manager: for instance, on macOS, you can install with the
  `brew install coreutils gmp` and on OpenBSD, with the
  `pkg_add coreutils bash gmake gmp` command. You specifically need the dev GMP
  library, which means on some systems the package you need to install will
  be named something more like `libgmp3-dev`.

* As a default, *pack* and all its managed libraries and binaries
  will be installed to `$HOME/.pack`. You can change this by setting
  the `PACK_DIR` environment variable to a different directory.

* Make sure that `$HOME/.pack/bin` is on your `PATH` and takes
  precedence over the bin folder(s) (if any) where existing versions of
  Idris2 are already installed.

  For instance:

  ```sh
  export PATH="$HOME/.pack/bin:$HOME/.idris2/bin:..."
  ```

## 2. Decide on a Package Collection to use

When installing *pack* for the first time, you have to tell the
installer which package collection you plan to use. For the time
being, it is best to go for the most recent nightly collection.
The packages listed therein have been verified to properly build
together, and you get to use an up-to-date version of the Idris2
compiler. You find a list of the current package collections
[here](https://github.com/stefan-hoeck/idris2-pack-db/tree/main/collections).

## 3. Install via *micropack*

After having installed all the necessary libraries and applications,
and having decided on a package collection to use, you are ready
to bootstrap the Idris compiler and set up *pack* and its root
directory. To make things easier, there is a pre-built, minimalistic
version of *pack* called *micropack* bundled with this repository.

Here's the installation command to get you started. This assumes
you decided on using package collection `nightly-220507`, and the
name of your Chez Scheme executable is `scheme`:

```sh
make micropack SCHEME=scheme DB=nightly-220507
```

Wait a couple of seconds. If *micropack* starts writing non-stop
to standard out, this means that bootstrapping of the Idris2
compiler has successfully started. This will take a couple of
minutes, so it's probably a good idea to go and have a cup of
coffee.

## 4. Verifying the Installation

You can verify that all went well by running the Idris2 executable:

```sh
idris2
```

This should start an Idris2 REPL session. You should also check that
*pack* has been installed properly. The following should print
the application's help text:

```sh
pack help
```

## 5. (Optional) Shell Auto-completion

*pack* supports tab auto-completion for Bash-like shells.

### For Bash Users

From within bash, run the following command:

```sh
eval "$(pack completion-script pack)"
```

You can also add it to your `.bashrc` file.

### For ZSH Users

From within ZSH, run the following commands:

```sh
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(pack completion-script pack)"
```

You can also add them to your `.zshrc` file.
