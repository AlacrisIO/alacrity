// vim: filetype=javascript

const init = (stdlib, wagerInEth, escrowInEth) => {
  const wagerInWei  = stdlib.toBN(stdlib.toWei(wagerInEth,  'ether'));
  const escrowInWei = stdlib.toBN(stdlib.toWei(escrowInEth, 'ether'));

  return Promise.resolve({ stdlib, gameState: { wagerInWei, escrowInWei }});
};

const play = (theRPS, drawFirst, interactWith) => ({ stdlib, gameState }) => {
  const { balanceOf, devnet, transfer, SC_createIdentity, deploy,
          MutableState, SpanCTC, random_uint256, uint256_to_bytes } = stdlib;
  const { prefundedDevnetAcct         } = devnet;
  const { wagerInWei, escrowInWei     } = gameState;

  const startingBalance = stdlib.toBN(stdlib.toWei('100', 'ether'));
  const deposit = stdlib.toBN(stdlib.toWei('1', 'ether'));

  const newPlayer = prefunder =>
    devnet.createAndUnlockAcct()
      .then(to => transfer(to, prefunder, startingBalance)
            .then(() => Promise.resolve(SC_createIdentity()))
            .then(sc_identity => [to, sc_identity]));
//                    .then(sc_identity => stdlib.EthereumNetwork(to, sc_identity)));

    const session = '0x' + uint256_to_bytes(random_uint256());
    const data = '0x' + uint256_to_bytes(random_uint256());

  const captureOpeningGameState = ([ a, b ]) =>
    Promise.all([ balanceOf(a), balanceOf(b) ])
    .then(([ balanceStartAlice, balanceStartBob ]) =>
    Object.assign(gameState
       , { alice: a
           , bob: b
           , list_nodes: [a, b]
           , ctors: [ a[0], b[0] ]
           , full_state: {session, clock: 0, participants: [a], data}
           , deposit
           , balanceStartAlice
           , balanceStartBob
         }));

  const captureClosingGameState = ([ outcomeBob, outcomeAlice ]) =>
    Promise.all([ balanceOf(gameState.alice), balanceOf(gameState.bob) ])
      .then(([ balanceEndAlice, balanceEndBob ]) =>
        Object.assign(gameState, { outcomeAlice
                                   , outcomeBob
                                   , balanceEndAlice
                                   , balanceEndBob
                                 }));


  const randomArray = a  => a[ Math.floor(Math.random() * a.length) ];
  const randomHand  = () => randomArray([ 'ROCK', 'PAPER', 'SCISSORS' ]);

  const makeDrawFirstHand = first => {
    let called = false;
    return () => {
      if (called) {
        return randomHand();
      } else {
        called = true;
        return first;
      }
    };
  };

  const shared = randomHand();

  const makeWhichHand = drawFirst
    ? () => makeDrawFirstHand(shared)
    : () => randomHand;

  const txn0 = { balance: 0, value: 0 };


  const specificShoot = contractAddress =>
    new Promise(resolve => {
        const mutStat = MutableState(contractAddress, gameState.ctors, gameState.bob, gameState.alice,
                                     gameState.deposit, gameState.full_state);
        const ctc = SpanCTC(mutStat);
        return Promise.race([ctc.SC_SpanThreads()]);
        resolve('foo');
    });


  const bobShoot = contractAddress =>
    new Promise(resolve => {
        const mutStat = MutableState(contractAddress, gameState.ctors, gameState.bob, gameState.alice,
                                     gameState.deposit, gameState.full_state);
        const ctc = SpanCTC(mutStat);
        return Promise.race([
            ctc.SC_SpanThreads(),
            ctc.SC_CreateSC(gameState.full_state)]);
    });
//                .then(() => theRPS.B(
//                    stdlib, ctc, txn0, interactWith('Bob', makeWhichHand())
//                    , resolve))]);



  const aliceShoot = contractAddress =>
        new Promise(resolve => {
            const mutStat = MutableState(contractAddress, gameState.ctors, gameState.alice, [0,0],
                                         gameState.deposit, gameState.full_state);
            const ctc = SpanCTC(mutStat);
            return Promise.race([
                ctc.SC_SpanThreads(),
                ctc.SC_CreateSC(gameState.full_state)]);
        });
//                    .then(() => theRPS.A(
//                        stdlib, ctc, txn0, interactWith('Alice', makeWhichHand())
//                        , wagerInWei, escrowInWei, resolve))]);

//    
//    .then(contractAddress => Promise.all([ specificShoot(contractAddress) ]))
//    .then(contractAddress => Promise.all([ bobShoot(contractAddress), aliceShoot(contractAddress) ]))
  return prefundedDevnetAcct()
        .then(p   => Promise.all([ newPlayer(p), newPlayer(p) ]))
        .then(captureOpeningGameState)
        .then(()  => deploy(gameState.alice[0])(gameState.full_state, gameState.ctors))
        .then(contractAddress => Promise.all([ aliceShoot(contractAddress) ]))
        .then(captureClosingGameState);
};

export const runGameWith = (theRPS, stdlib, doWhile, drawFirst, interactWith, wagerInEth, escrowInEth, uri) =>
  init(stdlib, wagerInEth, escrowInEth, uri)
    .then(play(theRPS, drawFirst, interactWith));
