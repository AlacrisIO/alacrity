Rock, Paper, Scissors Demo
==========================

To try this demo, first install and configure the [metamask](https://metamask.io) extension
for your browser (if you're using [Brave](https://brave.com/), it might be builtin).
Then, connect to the [Rinkeby test network](https://rinkeby.io/), and
use its [faucet](https://rinkeby.io/#faucet) to get some coins to play with.

To play the game as committed to the master branch on [github](https://github.com/alacrisio/alacrity/),
just point your browser to < https://alacrisio.github.io/alacrity/archive/rps-demo/ >
or equivalently < https://j.mp/RPSdemo2019 >.

To try this demo locally, make sure that `nginx` is installed,
and run `make start_nginx` to start it;
then direct your browser to < http://localhost:8088/archive/rps-demo/ >.

To hack on this demo, make sure that `make`, `solc`, `nodejs` and `npm` are installed.

To rebuild the contract, type:

    make

To install the Javascript dependencies for the test suite, use the command:

    make npm_install

To start a local ethereum network for testing
(with its own private ledger of test tokens), use the command:

    make run_ethereum

To deploy a contract on your local ethereum
(whether private or public network), use the command:

    make deploy

To run the tests (TBD), use the command:

    make test

To watch what messages the tests exchange on the wire with the `geth` node,
type the following command in another terminal,
and interrupt it with `Ctrl-C` when you're done,
leaving a trace in file `test.cap`:

    tcpdump -i lo -nn -s0 -A -w test.cap -v tcp port 8545

