pragma solidity ^0.5.2;

/*
  From the DSL to the solidity contract, many transformations and optimizations were manually done,
  that could be realistically done automatically.

  Necessary transformations include:
  - Transforming "musts" into "may post or may be timed out".
  - Adding a notion of timeout. The timeout applies to anyone who currently has a "must".
  - Adding a notion of escrow. The escrow applies to anyone who has a "must" in the future.

  Optimizations include:
  - Merging consecutive actions by the same participant (the initial actions by player 0):
    - creation of contract/context
    - sending money
    - initializing with a message
  - In the message exchange, specialize to the case of two people
    - the first one is merged as above.
    - only one is left, the second and last, that doesn't have to publish a commitment, just a hand
    - the salt of the second is not used, therefore can be omitted (can be automated)
  - Flattening the state
    - state is defined as plenty of global variables, sometimes undefined.
    - continuation closures were transformed into ADTs, closure conversion happened,
      linear analysis proved they could all be global in that contract.
    - after the closure contents have been converted,
      the program counter / current instruction / recipe step / machine state / etc. is represented as
      a single number (called "state") rather than a data structure.

 TODO:
 - Introduce an extra state Factory (3) so we can share code between factory and game,
   with the state being Factory in the factory, and 0, 1 or 2 in the game.
   (making 0 part of the game saves a little bit of gas at the initialization of each game)
 - Maybe make the contract a trivial proxy (see ../experiments/proxy.sol),
   and "just" delegatecall into the factory to do the changes?
   If the cost of contract creation dominates the cost of contract use as in r-p-s,
   that's worth it; not for long-running contract where the opposite is true, though.
 */

contract RockPaperScissors
{
        // Possible hands --
        // we can treeshake this definitions away, since it is not used in this file.
        enum Hand { Rock, Paper, Scissors }

        /** Current state of the state machine. */
        enum State {
            Waiting_for_player1,        // player0 funded wager+escrow and published a commitment
            Waiting_for_player0_reveal, // player1 showed his hand
            Completed                   // end of game (in the future, have a way to reset the contract to an Uninitialized state for repeated plays?)
        }
        State state;

        /** Timeout in blocks.
            This needs be low enough to be bearable by humans,
            yet large enough that attackers can't just DDoS you into timing out.
            Players can mutually agree on this parameter depending on the amount at stake.
            We recommend two days or more when running in production, two minutes and a half in test.
            The period between blocks being targeted to about 15 seconds on Ethereum,
            so this should be 11520 in production, and 10 in test.
         */
        uint timeout_in_blocks;

        /** Previous block at which state transition happened,
            for timeouts when waiting for a player */
        uint previous_block;

        address payable[2] player;

        bytes32 player0_commitment;
        uint wager_amount;
        uint escrow_amount;

        bytes32 salt;
        Hand hand0;
        Hand hand1;

        enum Outcome {
            Player1_wins,
            Draw,
            Player0_wins,
            Player1_wins_by_default,
            Player0_rescinds
        }
        // Outcome outcome; // NB: Superfluous: never read, can be computed purely on the client side.
        // at 5000 GAS, ~2 GWEI/GAS, 1e-9 ETH/GWEI, 250 USD/ETH, storing a 256-bit word of information in an ethereum contract costs about ¼¢ and a basic transaction (21000 GAS) costs about 1¢, so there's a lot of pennies to pinch by avoiding needless computation.

        // Utility functions

        // The current invocation MUST be done by player0.
        function require_player0 () view internal
        {
                require(player[0] == msg.sender);
        }

        // The current invocation MUST be done by player1.
        function require_player1 () view internal
        {
                require(player[1] == msg.sender);
        }

        // The current invocation be done by player1, or by anyone if the offer is open.
        function require_or_set_player1 () internal
        {
                if (player[1] == address(0)) {
                        player[1] = msg.sender;
                } else {
                        require_player1();
                }
        }

        // Player0 wins the given amount
        function player0_gets(uint _amount) internal
        {
                player[0].transfer(_amount);
        }

        // Player1 wins the given amount
        function player1_gets(uint _amount) internal
        {
                player[1].transfer(_amount);
        }

        // Timeout
        function require_timeout() view internal {
                require(block.number > previous_block + timeout_in_blocks);
        }

        //event Player0StartGame(address payable _player0, address payable _player1,
        //              uint _timeout_in_blocks, bytes32 _commitment,
        //              uint _wager_amount, uint _escrow_amount);

        // Constructor called by player0 when initializing the game.
        // player1 is the address of the opponent, or 0x0...0 for a game open to the first comer.
        // timeout in blocks specifies the timeout.
        // The commitment is a hash of some salt and the hand by player0.
        // The wager
        // We do NOT emit an event: an event will be emitted by factory contract.
        constructor (address payable _player0, address payable _player1,
                     uint _timeout_in_blocks, bytes32 _commitment, uint _wager_amount)
                public payable
        {
                require(msg.value > _wager_amount);

                // Initialize the game
                // The amount that a player gains if they win fairly,
                // or loses if they loose fairly
                wager_amount = _wager_amount;

                // The amount that player0 looses if they "cheat" by
                // not revealing their hand after they committed.
                // Player0 gets this back if they "play fair" by
                // revealing their hand, whether they win or lose.
                escrow_amount = msg.value - wager_amount;

                // The address of player0, including to transfer to
                // if player0 wins.
                player[0] = _player0;

                // If non-zero, restricts who player1 can be.
                player[1] = _player1;

                // Mutually agreeable timeout value
                timeout_in_blocks = _timeout_in_blocks;

                // Restricts what player0's hand can be: she will have to reveal and play
                // the preimage of this hash, chosen *before* player1 showed his hand.
                player0_commitment = _commitment;

                // Set the new state and previous_block
                state = State.Waiting_for_player1;
                previous_block = block.number;

                // Emit a relevant event.
                // -- NOPE: do it atomically with the contract creation --
                // because the existing web3 interface makes it especially hard to relate events
                // in an atomic way.
                // NB: the block number implicitly comes in the event itself.
                //emit Player0StartGame(_player0, _player1, _timeout_in_blocks,
                //                      _commitment, _wager_amount, escrow_amount);
        }

        event Player1ShowHand(address payable _player1, Hand hand1);

        // Function called by player1 when joining the game.
        // NB: player1 must show the key, address and amount MUST match, and the hand must be valid.
        function player1_show_hand (Hand _hand1) external payable
        {
                require(state == State.Waiting_for_player1);
                require_or_set_player1();
                require(msg.value == wager_amount);
                require(uint8(_hand1) < 3);

                hand1 = _hand1;

                // Set the new state and previous_block
                state = State.Waiting_for_player0_reveal;
                previous_block = block.number;

                // Emit a relevant event.
                // NB: the block number implicitly comes in the event itself.
                emit Player1ShowHand(player[1], _hand1);
        }

        event Player0Reveal(bytes32 _salt, Hand _hand0, Outcome outcome);

        // State 3, called by player0 after player1 played, reveals the committed hand.
        function player0_reveal (bytes32 _salt, Hand _hand0) external payable {
                require(state == State.Waiting_for_player0_reveal);
                require_player0();
                require(uint8(_hand0) < 3 && player0_commitment == keccak256(abi.encodePacked(_salt, _hand0)));

                // Compute outcome. 0: player1 wins, 1: draw, 2: player0 wins.
                // Numbers chosen to be one above the strcmp convention used by C, OCaml, Javascript, etc.
                // could be made a local variable. See above about outcome.
                Outcome outcome = Outcome((uint8(_hand0) + 4 - uint8(hand1)) % 3);

                if (outcome == Outcome.Player0_wins) {
                        // The reveal is in favor of player0
                        player0_gets(2*wager_amount+escrow_amount);
                } else if (outcome == Outcome.Player1_wins) {
                        // The reveal is in favor of player1
                        player1_gets(2*wager_amount);
                        player0_gets(escrow_amount);
                } else {
                        // The reveal is a draw
                        player1_gets(wager_amount);
                        player0_gets(wager_amount+escrow_amount);
                }
                //salt = _salt; // Not needed in production
                //hand0 = _hand0; // Not needed in production
                state = State.Completed; // Yes needed in production!

                // Emit a relevant event.
                // NB: the block number implicitly comes in the event itself.
                emit Player0Reveal(_salt, _hand0, outcome);
        }

        event Player0Rescind();

        // State 2 bis, rescind the offer to play -- called by player0 after player1 times out.
        function player0_rescind () external payable {
                require(state == State.Waiting_for_player1);
                require_player0();
                require_timeout();
                //outcome = Outcome.Player0_rescinds; // Superfluous, see above about outcome
                player0_gets(wager_amount+escrow_amount);
                state = State.Completed;
                emit Player0Rescind();
        }

        event Player1WinByDefault();

        // State 3 bis, win by default -- called by player1 after player0 times out rather than reveal hand.
        function player1_win_by_default () external payable {
                require(state == State.Waiting_for_player0_reveal);
                require_player1();
                require_timeout();
                //outcome = Outcome.Player1_wins_by_default; // Superfluous, see above about outcome
                player1_gets(2*wager_amount+escrow_amount);
                state = State.Completed;
                emit Player1WinByDefault();
        }
/* // Now commenting out this obsolete function.
        function query_state () external view
                returns (State _state, Outcome _outcome,
                         uint _timeout_in_blocks, uint _previous_block,
                         address _player0, address _player1,
                         bytes32 _player0_commitment,
                         uint _wager_amount, uint _escrow_amount,
                         bytes32 _salt, Hand _hand0, Hand _hand1) {
                return (state, outcome, timeout_in_blocks, previous_block, player[0], player[1],
                        player0_commitment, wager_amount, escrow_amount, salt, hand0, hand1);
        }
*/
}

contract RockPaperScissorsFactory
{
        // Include not just the contract address, but all the information about the game.
        // One might be tempted to log the game information separately, in an event in
        // the game itself, while the creation only creates the contract that points to it.
        // But the web3 interface will make it painful to relate two separate events,
        // because of issues with lack of atomicity in an asynchronous interface;
        // thus you'd have to track and relate multiple primary record identities until
        // they can be confirmed to be the same, or not.
        // Meanwhile, you shouldn't trust the game to be legit unless you know the txHash of the
        // contract creation, at which point you can directly check the event below.
        event Created(address _contract, address payable _player0, address payable _player1,
                      uint _timeout_in_blocks, bytes32 _commitment,
                      uint _wager_amount, uint _escrow_amount);

        function player0_start_game
                (address payable _player1, uint _timeout_in_blocks,
                 bytes32 _commitment, uint _wager_amount)
                public payable returns (RockPaperScissors _contract)
        {
                RockPaperScissors rpsContract =
                        (new RockPaperScissors).value(msg.value)
                        (msg.sender, _player1, _timeout_in_blocks, _commitment, _wager_amount);
                // NB: To save a few fractions of a cent, you could get the sender, address and value
                // from the transaction receipt.
                emit Created(address(rpsContract), msg.sender, _player1, _timeout_in_blocks,
                             _commitment, _wager_amount, msg.value - _wager_amount);
                return rpsContract;
        }
}

