import {init, registerGlobals} from "./common-utils.mjs";

import * as runtime from "./alacrity-runtime.mjs";
import * as backend from "./backend-auto.mjs";
import * as frontend from "./rps-frontend.mjs";

registerGlobals({runtime, backend, frontend})

init()

// Local Variables:
// mode: JavaScript
// End:
