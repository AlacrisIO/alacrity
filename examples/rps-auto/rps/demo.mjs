// vim: filetype=javascript

const init = (stdlib, wagerInEth, escrowInEth) => {
  const wagerInWei  = stdlib.toBN(stdlib.toWei(wagerInEth,  'ether'));
  const escrowInWei = stdlib.toBN(stdlib.toWei(escrowInEth, 'ether'));

  return Promise.resolve({ stdlib, gameState: { wagerInWei, escrowInWei }});
};

export const ROCK = 0;
export const PAPER = 1;
export const SCISSORS = 2;

export const B_WINS = 0;
export const DRAW = 1;
export const A_WINS = 2;

const play = (theRPS, drawFirst, interactWith) => ({ stdlib, gameState }) => {
  const { balanceOf, devnet, transfer } = stdlib;
  const { prefundedDevnetAcct         } = devnet;
  const { wagerInWei, escrowInWei     } = gameState;

  const startingBalance = stdlib.toBN(stdlib.toWei('100', 'ether'));

  const newPlayer = prefunder =>
    devnet.createAndUnlockAcct()
      .then(to => transfer(to, prefunder, startingBalance)
                    .then(() => stdlib.EthereumNetwork(to)));

  const captureOpeningGameState = ([ a, b ]) =>
    Promise.all([ balanceOf(a), balanceOf(b) ])
      .then(([ balanceStartAlice, balanceStartBob ]) =>
          Object.assign(gameState
                     , { alice: a
                       , bob:   b
                       , ctors: [ a.userAddress, b.userAddress ]
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
  const randomHand  = () => randomArray([ ROCK, PAPER, SCISSORS ]);

  const mkDrawFirstHand = first => {
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

  const mkWhichHand = drawFirst
    ? () => mkDrawFirstHand(shared)
    : () => randomHand;

  const txn0 = { balance: 0, value: 0 };

  // NB: Remember it's the _waiter_ who asserts their claim of a timeout
  // condition! In other words, the waiter's "<opponent> timed out"
  const awaitTimeoutFor = (ctc, label, participant, resolve) =>
    participant.onTimeout(ctc, label, addr =>
        addr  !== participant.userAddress ? resolve(`${label} timed out`)
      : label === 'Alice'                 ? resolve(`Bob timed out`)
      :                                     resolve(`Alice timed out`));


  const bobShoot = ctcAlice =>
    new Promise(resolve =>
      gameState.bob.attach(gameState.ctors, ctcAlice.address)
        .then(ctcBob => {
          awaitTimeoutFor(ctcBob, 'Bob', gameState.bob, resolve);
          return theRPS.B(stdlib
                        , ctcBob
                        , txn0
                        , interactWith('Bob', mkWhichHand())
                        , resolve);
        }));


  const aliceShoot = ctc =>
    new Promise(resolve => {
      awaitTimeoutFor(ctc, 'Alice', gameState.alice, resolve);
      return theRPS.A(stdlib
                    , ctc
                    , txn0
                    , interactWith('Alice', mkWhichHand())
                    , wagerInWei
                    , escrowInWei
                    , resolve);
    });


  return prefundedDevnetAcct()
    .then(p   => Promise.all([ newPlayer(p), newPlayer(p) ]))
    .then(captureOpeningGameState)
    .then(()  => gameState.alice.deploy(gameState.ctors))
    .then(ctc => Promise.all([ bobShoot(ctc), aliceShoot(ctc) ]))
    .then(captureClosingGameState);
};


export const timeoutPred =
  { never:       ()        => false
  , aliceForces: (name, a) => name == 'Alice' && a == 'reveals'
  , bobForces:   (name, a) => name == 'Bob'   && a == 'shows'
  };


export const runGameWith =
    ( theRPS
    , stdlib
    , doWhile
    , drawFirst
    , interactWith
    , wagerInEth
    , escrowInEth
    , uri
    ) =>
  init(stdlib, wagerInEth, escrowInEth, uri)
    .then(play(theRPS, drawFirst, interactWith));
