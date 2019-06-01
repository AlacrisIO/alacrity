'use strict';

import {isInBrowser, initialize} from "./common-utils.mjs";
import "./web3-prelude.mjs";
import "./local-storage.mjs";
import "./tinyqueue.mjs";
import "./common-runtime.mjs";
import "./common-ui.mjs";
import "./dapp-config.mjs";
import "./build/dapp-contract.mjs";
import "./dapp-backend.mjs";
import "./dapp-frontend.mjs";
import "./debug.mjs"

if (isInBrowser) {
    window.addEventListener('load', () => {
        console.log("Page loaded. Initializing...");
        return initialize()})}
else { initialize() }

// Local Variables:
// mode: JavaScript
// End:
