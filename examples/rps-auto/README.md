# Rock Paper Scissors via Alacrity

# Bootstrap
Ensure the following tools are installed on your system with the versions
listed (or greater for all but `z3`):
- `node` v10.16 (the latest "LTS" as of this writing)
- `npm` v6.9
- `stack` v2.1.1
- GNU `make` v4.1
- `geth` v1.8.21
- `z3` v4.8.4 (it's important to use this exact version for now)
- `solc` v0.5.2

We recommend you use
[`nvm` (node version manager)](https://github.com/nvm-sh/nvm#installation-and-update)
to install `node` and `npm`:
```bash
$ nvm install 10.16

# When swapping terminals or starting a fresh session you can ensure you're
# running the correct versions with:
$ nvm version
v8.2.0

$ nvm use 10.16
Now using node v10.16.0 (npm v6.9.0)
```

`stack` is similarly easy to install by following the directions
[here](https://docs.haskellstack.org/en/stable/README/#how-to-install).

How you install GNU `make`, `geth`, `z3` and `solc` will depend on your OS.

### macOS users running homebrew
The GNU `make`
[package](https://formulae.brew.sh/formula/make)
can be installed with:

```bash
$ brew install make
```

Follow the directions
[here](https://github.com/ethereum/homebrew-ethereum#installation)
to install `geth` and `solc`.

Homebrew also offers a `z3`
[package](https://formulae.brew.sh/formula/z3)
for which you'll need to specify a version:

```bash
brew install z3@4.8.4
```

### macOS users running macports


```bash
$ sudo port install gmake gnutar wget cmake
```

Fetch pre-built `z3` + `geth` packages from the web and stage them in a `~/bin`
(or similar) directory:

```bash
$ mkdir -p ~/bin && cd ~/bin

$ wget https://github.com/Z3Prover/z3/releases/download/z3-4.8.4/z3-4.8.4.d6df51951f4c-x64-osx-10.14.1.zip
$ wget https://gethstore.blob.core.windows.net/builds/geth-darwin-amd64-1.8.27-4bcc0a37.tar.gz

$ unzip z3-4.8.4.d6df51951f4c-x64-osx-10.14.1.zip
$ gnutar -xvf geth-darwin-amd64-1.8.27-4bcc0a37.tar.gz

# Update ~/.bashrc (or ~/.profile, etc), substituting `<you>` with your
# username, so the new binaries become visible in your $PATH:
export PATH="/Users/<you>/bin/geth-darwin-amd64-1.8.27-4bcc0a37:$PATH"
export PATH="/Users/<you>/bin/z3-4.8.4.d6df51951f4c-x64-osx-10.14.1/bin:$PATH"

# Activate the $PATH changes and verify the binaries are available now:
$ source ~/.bashrc

$ which geth
/Users/<you>/bin/geth-darwin-amd64-1.8.27-4bcc0a37/geth

$ which z3
/Users/<you>/bin/z3-4.8.4.d6df51951f4c-x64-osx-10.14.1/bin/z3
```

Follow the build instructions
[here](https://solidity.readthedocs.io/en/v0.5.10/installing-solidity.html#prerequisites-macos)
and
[here](https://solidity.readthedocs.io/en/v0.5.10/installing-solidity.html#clone-the-repository)
to build `solc` from source.

*TODO*: Fix the macOS `solc` instructions and add section for Debian.


# Tests
Invoke the following (in order) to run the test suite:

```bash
# (Will clear previous network state and restart `geth` if already running)
$ make run_ethereum

# Wait a moment to allow `geth` to catch up...

# (Re-)compile the contract ABI + bytecode
$ make contract

# Launch the test suite
$ make test

# Don't forget to shut down `geth` afterward so it's not taking up system
# resources unnecessarily
$ killall geth
```

*If `make test` fails with an `Error: Invalid JSON RPC response: undefined`
exception you should try again but wait a few moments between the
`run_ethereum` and `test` steps. The problem is that `geth` needs sufficient
time to launch the network before serving RPC requests.*

***Also note that you may need to replace `make` with `gmake` depending on your
setup.***


# Linting
The JavaScript code under the `rps/` directory can also be style-checked with
`make lint`. You won't see any output unless there are warnings and/or errors
to report.
