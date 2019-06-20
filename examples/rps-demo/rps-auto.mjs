import {registerGlobals} from "./common-prelude.mjs";
import {init, registerInit, identity, initFunctions} from "./common-utils.mjs";

import * as runtime from "./alacrity-runtime.mjs";
import * as backend from "./backend-auto.mjs";
//import * as frontend from "./rps-frontend.mjs";

registerInit({
    Frontend: {fun: identity, dependsOn: ["Backend"]},
});
delete initFunctions.WatchBlockchain;
delete initFunctions.WatchNewGames;
delete initFunctions.WatchActiveGames;


registerGlobals({runtime, backend, /*frontend*/ })

init()()
