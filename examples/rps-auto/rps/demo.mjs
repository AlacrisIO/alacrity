// vim: filetype=javascript

const init = (stdlib, wagerInEth, escrowInEth) => {
  const wagerInWei  = stdlib.toBN(stdlib.toWei(wagerInEth,  'ether'));
  const escrowInWei = stdlib.toBN(stdlib.toWei(escrowInEth, 'ether'));

  return Promise.resolve({ stdlib, gameState: { wagerInWei, escrowInWei }});
};

const play = (theRPS, drawFirst, interactWith) => ({ stdlib, gameState }) => {
  const { balanceOf, devnet, transfer, SC_createIdentity } = stdlib;
  const { prefundedDevnetAcct         } = devnet;
  const { wagerInWei, escrowInWei     } = gameState;

  const startingBalance = stdlib.toBN(stdlib.toWei('100', 'ether'));

  const newPlayer = prefunder =>
    devnet.createAndUnlockAcct()
      .then(to => transfer(to, prefunder, startingBalance)
                    .then(() => Promise.resolve(SC_createIdentity()))
                    .then(sc_identity => stdlib.EthereumNetwork(to,sc_identity)));

  const captureOpeningGameState = ([ a, b ]) =>
    Promise.all([ balanceOf(a), balanceOf(b) )]
      .then(([ balanceStartAlice, balanceStartBob ]) =>
          Object.assign(gameState
                     , { alice: a
                       , bob:   b
                       , list_nodes: [a.userAddress, b.userAddress]
                       , ctors: [ a.userAddress[0], b.userAddress[0] ]
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

  const bobShoot = contractAddress => (mypairAddress,initpairAddress) =>
    new Promise(resolve =>
      SpanMutableState(gameState.ctors, ctcAlice.address)
      .then(ctc => theRPS.B(stdlib
                            , ctc
                            , txn0
                            , interactWith('Bob', makeWhichHand())
                            , resolve)));

  const aliceShoot = ctc => (=>
    new Promise(resolve =>
      gameState.bob.attach(gameState.ctors, ctcAlice.address)
      theRPS.A(stdlib
             , ctc
             , txn0
             , interactWith('Alice', makeWhichHand())
             , wagerInWei
             , escrowInWei
             , resolve));

  return prefundedDevnetAcct()
    .then(p   => Promise.all([ newPlayer(p), newPlayer(p) ]))
    .then(captureOpeningGameState)
    .then(()  => gameState.alice.deploy(gameState.ctors))
    .then(ctc => Promise.all([ bobShoot(ctc), aliceShoot(ctc) ]))
    .then(captureClosingGameState);
};

export const runGameWith = (theRPS, stdlib, doWhile, drawFirst, interactWith, wagerInEth, escrowInEth, uri) =>
  init(stdlib, wagerInEth, escrowInEth, uri)
    .then(play(theRPS, drawFirst, interactWith));
