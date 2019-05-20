'use strict';

/** Configuration per network (indexed by networkID). */
// Map(string, string)
networkConfig =
  { "1": /* Metropolis, the Ethereum public PoW main network */
    {networkName: "Metropolis",
     contract: null, // TODO: create the main-chain contract after the code is stable enough.
     timeoutInBlocks: 11520,
     timeoutString: "about 48 hours",
     confirmationsWantedInBlocks: 100, // Play it safe: 25 minutes
     blockPollingPeriodInSeconds: 5,
     txExplorerUrl: "https://etherscan.io/tx/", // + ${tx} including 0x
     addressExplorerUrl: "https://etherscan.io/address/" }, // + ${tx} including 0x
    "4": /* Rinkeby, the public Geth-only PoA testnet */
    {networkName: "Rinkeby",
     contract:
     // with user-selectable timeout, from source at commit 2bb0e1977b57939e57b80a4d0aacc3cad8b4bc60
     {address: "0xd11ac337e5a79dd9ba7a5f62e24966af3229964d",
      creationHash: "0x173829649d2b39ecf2fae45c0578ef6c692d4d780b39a06ee3e55c5d9dff8001",
      creationBlock: 4410913},
     // Older contracts, with older code:
     // {address: "0x73e900e2f6b69a62a78cea31736f536d4076532b", creationHash: "0x2a37d19da8523b09ea507220874229f02c7d4ed20af9d977cf41b1239b0f6e2a", creationBlock: 4331637}, this version failed to properly save hand0 and hand1 (!!!)
     // {address: "0xa526caaabf917554bb49756409759fb0101e2ccd", creationHash: "0xf43b8c50018ec9a819e1022d9b6a7c07218a1a24e19260b6ca375d1cd34f6ec3", with 10 block timeout (very short!), from source at commit 68ceb3cfd3dbacf88c1df4152307572a357bc80e}
     // {address: "0x07326e7ec3935b4ff8411c08fa94deb0ad1c7e98", creationHash: "0x61a83184580f8bc3d5bc077578c7533073dbf4eff7cde23fe30795e3c1ccd766", with 10 block timeout (very short!), from source at commit 68ceb3cfd3dbacf88c1df4152307572a357bc80e, except with reverse winner logic - oops}
     // {address: "0xe7de0191a6f9683dd3f2011214e5533d00d604fe", creationHash: "0x47632009af8c66df628449f1a422d86aa3fd21d96ed93e050ca752bfcec2af93", with 11520 block timeout, from source at commit 99631383c862bea336afcc6bfbd615fd425834f9}
     timeoutInBlocks: 12,
     timeoutString: "about 3 minutes",
     confirmationsWantedInBlocks: 1,
     blockPollingPeriodInSeconds: 1,
     txExplorerUrl: "https://rinkeby.etherscan.io/tx/", // + ${tx} including 0x
     addressExplorerUrl: "https://rinkeby.etherscan.io/address/" // + ${tx} including 0x
    }
  };
