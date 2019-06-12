import {registerInit, isInBrowser, require, loggedAlert
       } from "./common-utils.mjs";

export let Web3;
export let web3; // a variable to store local web3 object
export let accounts;
export let userAddress;
export let networkId;
export let web3Provider;
export let crypto;

/** Get an identifier for the current network.
   TODO: when using web3 1.0, use web3.eth.net.getId().then(k) or whatever it exports,
   to also extract the chainId, which can notably distinguish between ETH and ETC
   (both claim to be networkId 1, but the latter uses chainId 61)
   : () => string */
export const getNetworkId = () => web3.version.network;

if (!isInBrowser) {
    Web3 = require("web3");
    crypto = require("crypto");
    const providerUrl = process.env.WEB3_PROVIDER || "http://localhost:8545"
    web3Provider = new Web3.providers.HttpProvider(providerUrl);
} else if (typeof window.web3 == 'undefined' || typeof window.ethereum == 'undefined') {
    alert('You need a Web3-compatible browser. Consider downloading the MetaMask extension.');
} else {
    crypto = window.crypto;
    Web3 = window.Web3;
    web3 = window.web3;
    web3Provider = window.web3.currentProvider;
}

if (web3Provider) {
    web3 = new Web3(web3Provider);
}

export const initWeb3 = k => {
    (kk => isInBrowser ? window.ethereum.enable().then(kk) : kk(web3.eth.accounts))(a => {
    accounts = a;
    // NB: assuming a call to .toLowercase() on the userAddress is redundant.
    userAddress = (web3.currentProvider && web3.currentProvider.selectedAddress) || accounts[0];
    networkId = getNetworkId();
    // logging("userAddress:", userAddress, "\naccounts:", accounts, "\nnetworkId:", networkId)();
    if (isInBrowser && !userAddress) {
        loggedAlert(`Your user address is undefined. \
Please reload this page with metamask enabled and an account selected.`);}
    return k()})}

registerInit({"Web3": {fun: initWeb3}})

// Local Variables:
// mode: JavaScript
// End:
