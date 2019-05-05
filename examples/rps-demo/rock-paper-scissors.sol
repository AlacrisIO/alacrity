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
        // At about 15 seconds per block, 11520 blocks is about two days for a timeout,
        // which should be safe against connectivity attacks.
        // For the sake of demo, I'll use 10 blocks (about 2.5 minutes) as the timeout.
        uint constant timeout_in_blocks = 10; // WARNING: DEMO ONLY

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

        /** Previous block at which state transition happened,
            for timeouts when waiting for a player */
        uint previous_block;

        address payable player0_address;
        bytes32 player0_commitment;
        uint wager_amount;
        uint escrow_amount;

        address payable player1_address;
        uint8 hand1;

        enum Outcome {
            Unknown,
            Draw,
            Player0_wins,
            Player1_wins,
            Player0_defaults,
            Player1_defaults,
            Player0_fails
        }
        Outcome outcome;

        // Utility functions

        // The current invocation MUST be done by player0.
        function require_player0 () view internal
        {
                require(player0_address == msg.sender);
        }

        // The current invocation MUST be done by player1.
        function require_player1 () view internal
        {
                require(player1_address == msg.sender);
        }

        // The current invocation be done by player1, or by anyone if the offer is open.
        function require_or_set_player1 () internal
        {
                if (player1_address == address(0)) {
                        player1_address = msg.sender;
                } else {
                        require_player1();
                }
        }

        // Player0 wins the given amount
        function player0_gets(uint _amount) internal
        {
                player0_address.transfer(_amount);
        }

        // Player1 wins the given amount
        function player1_gets(uint _amount) internal
        {
                player1_address.transfer(_amount);
        }

        // Timeout
        function require_timeout() view internal {
                require(block.number > previous_block + timeout_in_blocks);
        }

        // Constructor called by player0 when initializing the game.
        // The commitment is a hash of some salt and the hand.
        // The key_hash is a handshake so only trusted players can play.
        constructor (address payable _player0_address, address payable _player1_address,
                     bytes32 _commitment, uint _wager_amount)
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
                player0_address = _player0_address;

                // Restricts what player0's hand can be: she will have to reveal and play
                // the preimage of this hash, chosen *before* player1 showed his hand.
                player0_commitment = _commitment;

                // If non-zero, restricts who player1 can be.
                player1_address = _player1_address;

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

                player1_address = msg.sender;
                hand1 = _hand1;

                // Set the new state and previous_block
                state = State.Waiting_for_player0_reveal;
                previous_block = block.number;
        }

        // State 3, called by player0 after player1 played, reveals the committed hand.
        function player0_reveal (bytes32 salt, uint8 hand0) external payable {
                require(state == State.Waiting_for_player0_reveal);
                require_player0();
                require(hand0 < 3 && player0_commitment == keccak256(abi.encodePacked(salt, hand0)));

                // compute difference modulo 3 without underflowing
                uint8 diff = (hand1 + 3 - hand0) % 3;

                if (diff == 1) {
                        // The reveal is in favor of player0
                        outcome = Outcome.Player0_wins;
                        player0_gets(2*wager_amount+escrow_amount);
                } else if (diff == 2) {
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
                state = State.Completed;
        }

        // State 2 bis, rescind the offer to play -- called by player0 after player1 times out.
        function player0_rescind () external payable {
                require(state == State.Waiting_for_player1);
                require_player0();
                require_timeout();
                outcome = Outcome.Player1_defaults;
                player0_gets(wager_amount+escrow_amount);
                state = State.Completed;
        }

        // State 3 bis, win by default -- called by player1 after player0 times out rather than reveal hand.
        function player1_win_by_default () external payable {
                require(state == State.Waiting_for_player0_reveal);
                require_player1();
                require_timeout();
                outcome = Outcome.Player0_defaults;
                player1_gets(2*wager_amount+escrow_amount);
                state = State.Completed;
        }

        function query_state () external view
                returns (State _state, Outcome _outcome, uint _previous_block,
                         address _player0_address, address _player1_address,
                         bytes32 _player0_commitment,
                         uint _wager_amount, uint _escrow_amount, uint8 _hand1) {
                return (state, outcome, previous_block, player0_address, player1_address,
                        player0_commitment, wager_amount, escrow_amount, hand1);
        }
}

contract RockPaperScissorsFactory
{
        event Created(address _contract, address payable _player0, address payable _player1,
                      bytes32 _commitment, uint _wager_amount, uint _escrow_amount);

        function player0_start_game
                (address payable _player1_address, bytes32 _commitment, uint _wager_amount)
                public payable returns (address _contract_address)
        {
                RockPaperScissors rpsContract = (new RockPaperScissors).value(msg.value)(msg.sender, _player1_address, _commitment, _wager_amount);
                // NB: You have to get the address from the transaction receipt.
                emit Created(address(rpsContract), msg.sender, _player1_address,
                             _commitment, _wager_amount, msg.value - _wager_amount);
                return address(rpsContract);
        }

        // The debugging functions below were used to ascertain how to commitments are computed
        function compute_commitment_message (bytes32 _salt, uint8 _hand0)
                public pure returns (bytes memory _message) {
                return abi.encodePacked(_salt, _hand0);
        }
        function compute_commitment_digest (bytes32 _salt, uint8 _hand0)
                public pure returns (bytes32 _digest) {
                return keccak256(compute_commitment_message(_salt, _hand0));
        }
        event Commitment(bytes32 _hash, bytes _message);

        function compute_commitment (bytes32 _salt, uint8 _hand0)
                external returns (bytes32 _digest, bytes memory _message) {
                bytes memory message = compute_commitment_message(_salt, _hand0);
                bytes32 hash = keccak256(message);
                emit Commitment(hash, message);
                return (hash, message);
        }
}
