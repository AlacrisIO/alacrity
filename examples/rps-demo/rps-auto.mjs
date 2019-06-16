import {init, registerGlobals} from "./common-utils.mjs";

import * as alacrity_runtime from "./alacrity-runtime.mjs";
import * as rps_ala_backend from "./rps-ala-backend.mjs";
import * as rps_ala_frontend from "./rps-ala-frontend.mjs";

registerGlobals({alacrity_runtime, rps_ala_backend, rps_ala_frontend})

init()

// Local Variables:
// mode: JavaScript
// End:
