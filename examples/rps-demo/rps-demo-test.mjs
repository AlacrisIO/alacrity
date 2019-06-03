/* eslint-disable no-console */

import {initialize, initFunctions, registerInit,
        identity, logging, errbacK, assert
       } from "./common-utils.mjs";
import {web3, networkId, userAddress} from "./web3-prelude.mjs";
import "./local-storage.mjs";
import "./tinyqueue.mjs";
import {digestHex, confirmTransaction, config, checkContract} from "./common-runtime.mjs";
import "./common-ui.mjs";
import "./dapp-config.mjs";
import {rpsFactoryCode} from "./build/dapp-contract.mjs";
import {deployRps} from "./dapp-backend.mjs";

// TODO: implement a text "frontend". Have a flag to disable the auto-update daemon.
// import "./dapp-frontend.mjs";
// import "./debug.mjs"
// TODO: rename to cli, or have a cli file? import "./cli.mjs";

registerInit({
    Frontend: {fun: identity, dependsOn: ["Backend"]},
});
delete initFunctions.WatchBlockchain;
delete initFunctions.WatchNewGames;
delete initFunctions.WatchActiveGames;



const initialized = {};

const deployCommand = k =>
    initialize(["Backend"], initialized)(() => {
    console.log("Connected to network %s (%s) as %s", networkId, config.networkName, userAddress);
    web3.eth.defaultAccount = userAddress;
    errbacK(web3.eth.getBalance)(userAddress)(balance => {
    console.log("Account %s has balance %s", userAddress, balance);
    if (config.contract && digestHex(rpsFactoryCode) === config.contract.codeHash) {
        console.log("Contract already deployed at %s, not redeploying but checking it...",
                    config.contract.address);
        return checkContract(k);
    } else {
        deployRps(creationHash => {
            console.log("Deploying the contract through transaction %s...", creationHash);
            confirmTransaction(creationHash, 0)(() => {
            errbacK(web3.eth.getTransactionReceipt)(creationHash)(receipt => {
            assert(receipt.transactionHash === creationHash, "Bad tx hash");
            const address = receipt.contractAddress;
            const codeHash = digestHex(rpsFactoryCode);
            const creationBlock = receipt.blockNumber;
            console.log("Add this creation data to dapp-config.js for network %s: %s",
                        networkId, JSON.stringify({address, codeHash, creationHash, creationBlock}));
            console.log("You should wait for %d confirmations (%s)...",
                        config.confirmationsWantedInBlocks, config.confirmationsString)})})})}})})

const usageString = "Usage: undocumented, UTSL";
const versionString = "rps-demo 0.1";

let argStart = process.argv.indexOf("--") + 1;
let args = process.argv.slice(argStart);
// console.log("arguments: %s", JSON.stringify(args));
const processArg = () => {
    if (args.length === 0) {
        logging(usageString)(); process.exit(0);
    } else {
        switch (args[0]) {
        case "version": logging(versionString)(); process.exit(0); break;
        case "help": logging(usageString)(); process.exit(0); break;
        case "deploy": deployCommand(identity); break;
        default: console.error("%s", usageString); process.exit(2); break;}}}

processArg();

// Local Variables:
// mode: JavaScript
// End:
