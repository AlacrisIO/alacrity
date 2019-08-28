#!/bin/sh -eu
# run the Ethereum test net

# Identify where to run things from where this script is.
HERE=$(dirname "$0")
cd "$HERE/../../" # Change to toplevel directory of git repository
TOPDIR=$(pwd) # Top directory for the git repository

# First, kill any previously existing geth
killall geth > /dev/null 2>&1 || true


GETH_RUNDIR=$TOPDIR/_ethereum
mkdir -p $GETH_RUNDIR
cd $GETH_RUNDIR

PORT=30303
RPCPORT=8545
DATADIR=geth-data

# clean out any existing data dir, thereby resetting eth blockchain state.
if [ -d $DATADIR ]; then
    rm -rf $DATADIR
fi

mkdir $DATADIR

# Logs used to be under LOGDIR=$TOPDIR/_run/logs/ but then they were erased by make clean,
# so instead put them in under DATADIR.
LOGDIR=$DATADIR/logs
mkdir -p $LOGDIR

V1_9_X_ARGS=$(geth version 2>/dev/null |grep -q "^Version: 1.9" && echo "--allow-insecure-unlock" || echo "")

# Note: `--dev.period 12` is used to prevent the `geth` miner pausing in the
# absence of pending transactions to process (e.g. in the case of demo/test
# suite timeouts) and "12" has been chosen specifically to simulate the nominal
# block generation speed design goal one should expect to see on the main net:
# https://blog.ethereum.org/2014/07/11/toward-a-12-second-block-time/

# TODO provide option to easily run w/ `--dev.period 1` for quicker demos and
# to demonstrate that timing is truly pinned to block generation (vs. clock
# speed)
geth ${V1_9_X_ARGS} \
  --dev \
  --dev.period=12 \
  --mine \
  --identity "AlacrisEthereumDevNet" \
  --datadir $DATADIR \
  --nodiscover \
  --maxpeers 0 \
  --rpc --rpcapi "db,eth,net,debug,web3,light,personal,admin" --rpcport $RPCPORT --rpccorsdomain "*" \
  --port $PORT \
  --nousb \
  --networkid 17 \
  --nat "any" \
  --ipcpath .ethereum/geth.ipc \
  > $LOGDIR/testnet.log 2>&1 &

while ! curl -sSf -X POST \
  -H "Content-Type: application/json" \
  --data '{"jsonrpc":"2.0", "method": "web3_clientVersion", "params":[], "id":67}' http://localhost:$RPCPORT ; do
    echo "Geth not started yet, waiting..."
    sleep 1
done
