pragma solidity ^0.5.2;

// TODO: Should we emit an event for each transition,
// or rely on people watching the chain?

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
        uint constant timeout_in_blocks = 11520;

        // Possible hands
        uint8 constant rock = 0;
        uint8 constant paper = 1;
        uint8 constant scissors = 2;
        uint8 constant none = 255;

        /** Current state of the state machine. Possible States:
            0 - uninitialized
            1 - player0 funded wager+escrow and published a commitment; waiting for player1
            2 - player1 showed his hand; waiting for player0
            255 - end of game (in the future, have a way to reset the contract to state 0?)
         */
        uint8 state = 0;
        uint8 constant uninitialized = 0;
        uint8 constant waiting_for_player1 = 1;
        uint8 constant waiting_for_reveal = 2;
        uint8 constant completed = 255;

        /** Previous block at which state transition happened,
            for timeouts when waiting for a player */
        uint previous_block;

        address payable player0_address;
        bytes32 player0_commitment;
        uint wager_amount;
        uint escrow_amount;
        bytes32 key_hash;

        address payable player1_address;
        uint8 hand1;

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

        // State 1, called by player0 when initializing the game
        // Let the address be 0 if this is an open offer, or the address of a specific opponent.
        // The commitment is a hash of some salt and the hand.
        constructor (bytes32 _commitment, bytes32 _key_hash, uint _wager_amount) public payable
        {
                // This function can only be called while at state 0.
                require(state == uninitialized);
                require(msg.value > _wager_amount);

                // Initialize the game
                wager_amount = _wager_amount;
                escrow_amount = msg.value - wager_amount;
                player0_address = msg.sender;
                player0_commitment = _commitment;
                key_hash = _key_hash;

                // Set the new state and previous_block
                state = waiting_for_player1;
                previous_block = block.number;
        }

        // State 2, called by player1 when joining the game
        // NB: the player1 address and amount MUST match, and the hand must be valid.
        function player1_show_hand (bytes32 key, uint8 _hand1) external payable
        {
                require(state == waiting_for_player1);
                require(keccak256(abi.encodePacked(key)) == key_hash);
                require(msg.value == wager_amount);
                require(_hand1 < 3);

                player1_address = msg.sender;
                hand1 = _hand1;

                // Set the new state and previous_block
                state = waiting_for_reveal;
                previous_block = block.number;
        }

        // State 3, called by player0 after player1 played, reveals the committed hand.
        function player0_reveal (bytes31 salt, uint8 hand0) external payable {
                require(state == waiting_for_reveal);
                require_player0();
                if (hand0 >= 3 || player0_commitment != keccak256(abi.encodePacked(salt, hand0))) {
                        // If the reveal is invalid, player1 wins and player0 loses escrow.
                        player1_gets(2*wager_amount+escrow_amount);
                } else {
                        // compute difference modulo 3 without underflowing
                        uint8 diff = (hand0 + 3 - hand1) % 3;

                        if (diff == 1) {
                                // The reveal is in favor of player0
                                player0_gets(2*wager_amount+escrow_amount);
                        } else if (diff == 2) {
                                // The reveal is in favor of player1
                                player1_gets(2*wager_amount);
                                player0_gets(escrow_amount);
                        } else {
                                // The reveal is a draw
                                player1_gets(wager_amount);
                                player0_gets(wager_amount+escrow_amount);
                        }
                }
                state = completed;
        }

        // State 2 bis, rescind the offer to play -- called by player0 after player1 times out.
        function player0_rescind () external payable {
                require(state == waiting_for_player1);
                require_player0();
                require_timeout();
                player0_gets(wager_amount+escrow_amount);
                state = completed;
        }

        // State 3 bis, win by default -- called by player1 after player0 times out rather than reveal hand.
        function player1_win_by_default () external payable {
                require(state == waiting_for_reveal);
                require_player1();
                require_timeout();
                player1_gets(wager_amount+escrow_amount);
                state = completed;
        }
}

contract RockPaperScissorsFactory
{
        function createRockPaperScissors
                (bytes32 _commitment, bytes32 _key_hash, uint _wager_amount) public payable
        {
                (new RockPaperScissors).value(msg.value)(_commitment, _key_hash, _wager_amount);
                // NB: Not returning the address; you have to get it from the transaction receipt.
        }
}
