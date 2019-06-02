Rock, Paper, Scissors Demo
==========================

To try this demo as committed to the master branch on github,
just point your browser to < https://j.mp/RPSdemo2019a >,
and, after installing and configuring the [metamask](https://metamask.io) extension
to your browser, connect to the [Rinkeby test network](https://rinkeby.io/)
— you may have to use the [faucet](https://rinkeby.io/#faucet) to get some coins to play with.

To try this demo locally, make sure that `nginx` is installed,
and run `make start_nginx` to start it, then
direct your browser to < http://localhost:8088/rps-demo/ >.

To hack on this demo, make sure that `make`, `solc`, `nodejs` and `npm` are installed.

To rebuild the contract, type: `make`

To install the Javascript dependencies for the test suite, type: `make npm_install`

To run the tests (TBD), type: `make test`
