# Rock Paper Scissors via Alacrity

# Bootstrap
Ensure you have `node` v10 (or greater) and `npm` v6 (or greater) installed and
available on your `$PATH`, then `npm clean-install` to fetch the necessary JS
dependencies.

You'll also need `geth` in order to run a private Ethereum network if you'd
like to run the tests, and GNU Make v4 (or greater) to drive the Makefile
targets described below.

# Tests
Invoke the following (in order) to run the test suite:
- `make run_ethereum` (will clear previous network state and restart if already
  running)
- `make contract`
- `make test`

*If `make test` fails with an `Error: Invalid JSON RPC response: undefined`
exception you should try again but wait a few moments between the
`run_ethereum` and `test` steps. The problem is that `geth` needs sufficient
time to launch the network before serving RPC requests.*


# Linting
The JavaScript code under the `rps/` directory can also be style-checked with
`make lint`. You won't see any output unless there are warnings and/or errors
to report.
