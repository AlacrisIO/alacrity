import {init, registerGlobals} from "./common-utils.mjs";

import * as alacrity_runtime from "./alacrity-runtime.mjs";
import * as dapp_backend from "./dapp-backend.mjs";
import * as dapp_frontend from "./dapp-frontend.mjs";

registerGlobals({alacrity_runtime, dapp_backend, dapp_frontend})

init()

// Local Variables:
// mode: JavaScript
// End:
