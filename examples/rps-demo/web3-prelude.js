// Web3 browser user detected. You can now use the provider.

var web3js; // a variable to store local web3 object
var accounts;

if (typeof window.web3 == 'undefined' || typeof window.ethereum == 'undefined') {
    alert('You need a Web3-compatible browser. Consider downloading the MetaMask extension.');
} else {
    // Initialize Web3 object as injected by Ethereum compatible browser such as MetaMask/Mist
    // Some people put the below inside a
    window.addEventListener('load', () => {
        web3js = new Web3(web3.currentProvider);
        ethereum.enable().then((x) => {
            accounts = x;
            initBackend(initFrontend);
        });});}
