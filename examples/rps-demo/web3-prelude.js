// Web3 browser user detected. You can now use the provider.

if (typeof window.web3 == 'undefined' || typeof window.ethereum == 'undefined') {
    alert('You need a Web3-compatible browser. Consider downloading the MetaMask extension.');
}
