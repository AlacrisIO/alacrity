import { isInBrowser, initialize, registerInit } from './common-utils.mjs';
import * as R from './common-runtime.mjs';
import './web3-prelude.mjs';
import './local-storage.mjs';
import './tinyqueue.mjs';
import './dapp-config.mjs';
import './build/dapp-contract.mjs';
import './dapp-backend.mjs';

const registerFrontendDeps = () => registerInit(
  { Games:            { fun: R.initGames,        dependsOn: ['Frontend'                       ] }
  , ResumeGames:      { fun: R.resumeGames,      dependsOn: ['Games'                          ] }
  , WatchNewGames:    { fun: R.watchNewGames,    dependsOn: ['ResumeGames', 'WatchBlockchain' ] }
  , WatchActiveGames: { fun: R.watchActiveGames, dependsOn: ['WatchNewGames'                  ] }
  });

if (isInBrowser) {
  window.addEventListener('load', () =>
    import('./common-ui.mjs')
      .then(() => import('./dapp-frontend.mjs'))
      .then(() => import('./debug.mjs'))
      .then(() => {
        registerFrontendDeps();
        /* eslint-disable no-console */
        console.log('Page loaded. Initializing...');
        return initialize()();
      }));

} else {
  initialize()()
}

// vim: filetype=javascript
// Local Variables:
// mode: JavaScript
// End:
