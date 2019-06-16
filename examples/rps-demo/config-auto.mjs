/** The networkConfig is an object that maps the networkId (TODO: in the future, plus chain Id?)
    to the config below. It is the responsibility of the DApp developers to define the networkConfig
    as part of their DApp deployment, typically in a dapp-config.mjs file loaded from the HTML page.
    : StringTable(config)
 */
// Map(string, string)
export const networkConfig =
  { "1": /* Metropolis, the Ethereum public PoW main network */
    {networkName: "Metropolis",
     networkUrl: "https://etherscan.io/",
     contract: null, // TODO: create the main-chain contract after the code is stable enough.
     timeoutInBlocks: 11520,
     timeoutString: "about 48 hours",
     confirmationsWantedInBlocks: 100, // Play it safe: 25 minutes
     confirmationsString: "about 25 minutes",
     blockPollingPeriodInSeconds: 5,
     txExplorerUrl: "https://etherscan.io/tx/", // + ${tx} including 0x
     addressExplorerUrl: "https://etherscan.io/address/" }, // + ${tx} including 0x
    "4": /* Rinkeby, the public Geth-only PoA testnet */
    {networkName: "Rinkeby",
     networkUrl: "https://www.rinkeby.io/",
     contract: null, // TODO: deploy it!
     timeoutInBlocks: 12,
     timeoutString: "about 3 minutes",
     confirmationsWantedInBlocks: 1,
     confirmationsString: "about 15 seconds",
     blockPollingPeriodInSeconds: 1,
     txExplorerUrl: "https://rinkeby.etherscan.io/tx/", // + ${tx} including 0x
     addressExplorerUrl: "https://rinkeby.etherscan.io/address/" // + ${tx} including 0x
    },
    "17": /* devnet, a private Geth PoA testnet running on localhost */
    {networkName: "devnet",
     networkUrl: null, // TODO: deploy etherscan on the private network!
     contract: null,
     timeoutInBlocks: 12,
     timeoutString: "after a dozen transactions are issued",
     confirmationsWantedInBlocks: 1,
     confirmationsString: "as soon as another transaction is issued",
     blockPollingPeriodInSeconds: 1,
     txExplorerUrl: null, // TODO: deploy etherscan on the private network!
     addressExplorerUrl: null, // TODO: deploy etherscan on the private network!
    }
  };

// Local Variables:
// mode: JavaScript
// End:
