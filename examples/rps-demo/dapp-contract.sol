pragma solidity ^0.5.2;

// TODO: Should we emit an event for each transition,
// or rely on people watching the chain transactions and/or running the query function?

/*
  From the DSL to the solidity contract, many optimizations were manually done,
  that could be realistically done automatically, including:
  - merging consecutive actions by the same participant (the initial actions by player 0):
    - creation of contract/context
    - sending money
    - initializing with a message
  - in the message exchange, specialize to the case of two people
    - the first one is merged as above.
    - only one is left, the second and last, that doesn't have to publish a commitment, just a hand
    - the salt of the second is not used, therefore can be omitted (can be automated)
  - flattening the state
    - state is defined as plenty of global variables, sometimes undefined.
    - continuation closures were transformed into ADTs, closure conversion happened,
      linear analysis proved they could all be global in that contract.
    - after the closure contents have been converted,
      the program counter / current instruction / recipe step / machine state / etc. is represented as
      a single number (called "state") rather than a data structure.
 */

contract RockPaperScissors
{
        // Possible hands
        uint8 constant rock = 0;
        uint8 constant paper = 1;
        uint8 constant scissors = 2;

        /** Current state of the state machine. */
        enum State {
            Uninitialized,
            Waiting_for_player1,        // player0 funded wager+escrow and published a commitment
            Waiting_for_player0_reveal, // player1 showed his hand
            Completed                   // end of game (in the future, have a way to reset the contract to state Uninitialized?)
        }
        State state = State.Uninitialized;

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
        uint8 hand0;
        uint8 hand1;

        enum Outcome {
            Unknown,
            Draw,
            Player0_wins,
            Player1_wins,
            Player1_wins_by_default,
            Player0_rescinds
        }
        Outcome outcome;

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

        // Constructor called by player0 when initializing the game.
        // The commitment is a hash of some salt and the hand.
        // The key_hash is a handshake so only trusted players can play.
        constructor (address payable _player0, address payable _player1,
                     uint _timeout_in_blocks, bytes32 _commitment, uint _wager_amount)
                public payable
        {
                // This function can only be called while at state Uninitialized.
                require(state == State.Uninitialized);
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
        }

        // Function called by player1 when joining the game.
        // NB: player1 must show the key, address and amount MUST match, and the hand must be valid.
        function player1_show_hand (uint8 _hand1) external payable
        {
                require(state == State.Waiting_for_player1);
                require_or_set_player1();
                require(msg.value == wager_amount);
                require(_hand1 < 3);

                hand1 = _hand1;

                // Set the new state and previous_block
                state = State.Waiting_for_player0_reveal;
                previous_block = block.number;
        }

        // State 3, called by player0 after player1 played, reveals the committed hand.
        function player0_reveal (bytes32 _salt, uint8 _hand0) external payable {
                require(state == State.Waiting_for_player0_reveal);
                require_player0();
                require(_hand0 < 3 && player0_commitment == keccak256(abi.encodePacked(_salt, _hand0)));

                // compute result. 0: player1 wins, 1: draw, 2: player0 wins.
                uint8 diff = (_hand0 + 4 - hand1) % 3;

                if (diff == 2) {
                        // The reveal is in favor of player0
                        outcome = Outcome.Player0_wins;
                        player0_gets(2*wager_amount+escrow_amount);
                } else if (diff == 0) {
                        // The reveal is in favor of player1
                        outcome = Outcome.Player1_wins;
                        player1_gets(2*wager_amount);
                        player0_gets(escrow_amount);
                } else {
                        // The reveal is a draw
                        outcome = Outcome.Draw;
                        player1_gets(wager_amount);
                        player0_gets(wager_amount+escrow_amount);
                }
                salt = _salt;
                hand0 = _hand0;
                state = State.Completed;
        }

        // State 2 bis, rescind the offer to play -- called by player0 after player1 times out.
        function player0_rescind () external payable {
                require(state == State.Waiting_for_player1);
                require_player0();
                require_timeout();
                outcome = Outcome.Player0_rescinds;
                player0_gets(wager_amount+escrow_amount);
                state = State.Completed;
        }

        // State 3 bis, win by default -- called by player1 after player0 times out rather than reveal hand.
        function player1_win_by_default () external payable {
                require(state == State.Waiting_for_player0_reveal);
                require_player1();
                require_timeout();
                outcome = Outcome.Player1_wins_by_default;
                player1_gets(2*wager_amount+escrow_amount);
                state = State.Completed;
        }

        function query_state () external view
                returns (State _state, Outcome _outcome,
                         uint _timeout_in_blocks, uint _previous_block,
                         address _player0, address _player1,
                         bytes32 _player0_commitment,
                         uint _wager_amount, uint _escrow_amount,
                         bytes32 _salt, uint8 _hand0, uint8 _hand1) {
                return (state, outcome, timeout_in_blocks, previous_block, player[0], player[1],
                        player0_commitment, wager_amount, escrow_amount, salt, hand0, hand1);
        }
}

contract RockPaperScissorsFactory
{
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
                // NB: You have to get the address from the transaction receipt.
                emit Created(address(rpsContract), msg.sender, _player1, _timeout_in_blocks,
                             _commitment, _wager_amount, msg.value - _wager_amount);
                return rpsContract;
        }
}
