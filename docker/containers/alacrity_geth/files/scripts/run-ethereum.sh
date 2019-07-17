#!/bin/sh

cd "/var/www/app/alacrity-geth" # Change to toplevel directory of legicash-facts

LOGDIR=/var/www/app/alacrity-geth/_run/logs/

GETH_RUNDIR=$(pwd)/_ethereum

cd $GETH_RUNDIR

# run the Ethereum test net

PORT=30303
RPCPORT=8545
DATADIR=geth-data

# clean out existing data dir
if [ -d $DATADIR ]; then
    rm -rf $DATADIR
fi

mkdir $DATADIR

# kill any existing geth
killall geth > /dev/null 2>&1 || true

geth \
    --dev \
    --mine \
    --identity "AlacrisEthereumDevNet" \
    --datadir $DATADIR \
    --nodiscover \
    --maxpeers 0 \
    --rpcaddr "0.0.0.0" \
    --rpc --rpcapi "db,eth,net,debug,web3,light,personal,admin" \
    --rpcport $RPCPORT \
    --rpccorsdomain "*" \
    --rpcvhosts "*" \
    --port $PORT \
    --networkid 17 \
    --nat "any" \
    --ipcpath .ethereum/geth.ipc \
    > $LOGDIR/testnet.log 2>&1
