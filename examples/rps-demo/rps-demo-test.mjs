/* eslint-disable no-console */

import {initialize, initFunctions, registerInit,
        identity, logging, errbacK, assert, kLogResult, kLogError
       } from "./common-utils.mjs";
import {web3, networkId, userAddress} from "./web3-prelude.mjs";
import "./local-storage.mjs";
import "./tinyqueue.mjs";
import {digestHex, confirmTransaction, config, checkContract} from "./common-runtime.mjs";
import "./common-ui.mjs";
import "./dapp-config.mjs";
import {contractFactoryCode} from "./build/dapp-contract.mjs";
import {deployFactory} from "./dsl-api.mjs";

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

const deployCommand = (k = kLogResult, kError = kLogError) => {
    initialize(["Backend"], initialized)(() => {
    console.log("Connected to network %s (%s) as %s", networkId, config.networkName, userAddress);
    web3.eth.defaultAccount = userAddress;
    //errbacK(web3.eth.getBalance)(userAddress)(balance => {
    //console.log("Account %s has balance %s", userAddress, balance);
    //const len = (rpsFactoryCode.length-2)/2;
    //const intrinsicCost = 21000 + len*68; // NB: zero bytes actually only cost 4.
    //console.log("Code has length %d, intrinsic cost %d", len, intrinsicCost); // 3291, 244788
    //errbacK(web3.eth.getBlock)("pending")(block => {
    //console.log("Block has gas limit %s", block.gasLimit); // 6289319

    const kDeploy = () =>
        deployFactory(creationHash => {
            console.log("Deploying the contract through transaction %s...", creationHash);
            return confirmTransaction(creationHash, 0)(() =>
            errbacK(web3.eth.getTransactionReceipt)(creationHash)(receipt => {
            assert(receipt.transactionHash === creationHash, "Bad tx hash");
            const address = receipt.contractAddress;
            const codeHash = digestHex(rpsFactoryCode);
            const creationBlock = receipt.blockNumber;
            // TODO: add it ourselves, atomically enough.
            console.log("PLEASE ADD THIS CONTRACT CREATION RECORD to dapp-config.js for network %s:\n%s",
                        networkId, JSON.stringify({address, codeHash, creationHash, creationBlock}));
            console.log("Now waiting for %d confirmations (%s) — use Ctrl-C to interrupt earlier.",
                        config.confirmationsWantedInBlocks, config.confirmationsString);
            return confirmTransaction(creationHash, config.confirmationsWantedInBlocks)(
                k)}, kError))})
    if (config.contract && digestHex(rpsFactoryCode) === config.contract.codeHash) {
        console.log("Contract already deployed at %s, not redeploying but checking it...",
                    config.contract.address);
        return checkContract(k, err => {console.log("Contract invalid: %s", err); return kDeploy()});
    }
    return kDeploy()})}

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

// vim: filetype=javascript
// Local Variables:
// mode: JavaScript
// End:
