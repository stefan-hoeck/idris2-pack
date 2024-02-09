# Installing

This section describes in detail the installation
procedure for installing *pack* for the first time.
Although it is possible to use an existing Idris2
compiler to build and install pack, it is best to use
the installation script bundled with this project.
As an alternative, the pre-built *micropack* installer
can be used.

## 1. Preparations

The following libraries and applications are required
for working with Idris2. Make sure you have them installed
before you continue:

* `git` version >= 2.35.1. Earlier versions of `git` will cause issues
  when caching GitHub repositories locally.

* A Scheme compiler; either [Chez Scheme](https://cisco.github.io/ChezScheme/)
  (default), or Racket.
  If you install Chez Scheme from source files, building it locally,
  make sure you run `./configure --threads` to build multithreading support in.

  Note: On Apple Silicon, Chez Scheme works out-of-the-box as of version 10.0.0,
  and `brew install chezscheme` should install the latest version. If for some
  reason you cannot upgrade your Chez version, see the
  [Apple M1 User](#apple-m1-user) appendix.

* `bash`, `GNU make`, `gcc` or `clang`, `sha256sum`, and `GMP`. On Linux, you probably
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
  export PATH="$HOME/.pack/bin:$HOME/.idris2/bin:$PATH"
  ```

## 2. Install via the `install.bash` shell script

After having installed all the necessary libraries and applications,
and having decided on a package collection to use, you are ready
to bootstrap the Idris compiler and set up *pack* and its root
directory. To make things easier, there is shell script to do
all this for you bundled with this repository.

Here's the installation command to get you started.

```sh
bash -c "$(curl -fsSL https://raw.githubusercontent.com/stefan-hoeck/idris2-pack/main/install.bash)"
```

You will be asked about the name of your Chez Scheme executable,
so make sure to have this information ready.

Installation will take a couple of minutes, during which the
script will download and bootstrap the Idris compiler, before
building pack and its dependencies and setting everything up
to use the latest package collection.

If you don't have `curl` installed, you can - as an alternative -
clone this GitHub repository and execute the shell script like so:

```sh
git clone https://github.com/stefan-hoeck/idris2-pack.git pack
bash -c pack/install.bash
```
### 2.1. Installation via *micropack*

As an alternative to the installation script described above,
you can use the pre-built *micropack* installer bundled with
this repository. This will install the latest nightly package
collection (the list of available package collections can be found
[here](https://github.com/stefan-hoeck/idris2-pack-db/tree/main/collections)).

Here's the installation command to get you started. This assumes
the name of your Chez Scheme executable is `scheme`:

```sh
make micropack SCHEME=scheme
```

In order to use racket instead of chez scheme, run the
following:

```sh
make micropack-racket
```

Wait a couple of seconds. If *micropack* starts writing non-stop
to standard out, this means that bootstrapping of the Idris2
compiler has successfully started. This will take a couple of
minutes, so it's probably a good idea to go and have a cup of
coffee.

## 3. Verifying the Installation

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

## 4. (Optional) Shell Auto-completion

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


## Apple M1 user

Apple Silicon processors (M1, M2, etc.) are supported as of Chez Scheme 10.0.0,
released on 2024-02-07, and `brew install chezscheme` should Just Work.

If for some reason you cannot upgrade your Chez version, the easiest way to get
an Apple Silicon compatible install is to use the
[Racket fork of Chez](https://github.com/racket/ChezScheme).
When configuring the Chez Scheme build, Idris2 requires threading support to be
enabled, so make sure to pass the `--threads` flag to Chez's `./configure`.

For example:

```sh
git clone 'https://github.com/racket/ChezScheme.git'
cd ChezScheme
./configure --threads
make
sudo make install
```
