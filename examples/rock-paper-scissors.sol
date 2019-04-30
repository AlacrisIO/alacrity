pragma solidity ^0.5.2;

contract RockPaperScissors
{
        // At about 15 seconds per block, 11520 blocks is about two days for a timeout,
        // which should be safe against connectivity attacks.
        uint constant timeout_in_blocks = 11520;

        uint8 constant rock = 0;
        uint8 constant paper = 1;
        uint8 constant scissors = 2;
        uint8 constant none = 255;

        uint8 step = 0;
        uint amount;
        address payable player0_address;
        bytes32 player0_commitment;
        address payable player1_address;
        uint8 hand1;
        uint previous_block;

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

        // The current invocation MUST be done by player1.
        // If it is open to any player1, then set the player1 address.
        function require_or_set_player1 () internal
        {
                if (player1_address == address(0)) {
                        player1_address = msg.sender;
                } else {
                        require_player1();
                }
        }

        // Player0 wins the given amount
        function player0_wins(uint _amount) internal
        {
                player0_address.transfer(_amount);
        }

        // Player1 wins the given amount
        function player1_wins(uint _amount) internal
        {
                player1_address.transfer(_amount);
        }

        // Player0 and player1 both win the given amount
        function draw(uint _amount) internal
        {
                player0_wins(_amount);
                player1_wins(_amount);
        }

        // Timeout
        function require_timeout() view internal {
                require(block.number > previous_block + timeout_in_blocks);
        }

        // Step 1, called by player0 when initializing the game
        // Let the address be 0 if this is an open offer, or the address of a specific opponent.
        // The commitment is a hash of some salt and the hand.
        function fund (bytes32 _commitment, address payable _player1) external payable
        {
                // This function can only be called while at step 0.
                require(step == 0);

                // Initialize the game
                amount = msg.value;
                player0_address = msg.sender;
                player0_commitment = _commitment;
                player1_address = _player1;

                // Set the step to 1, the block to block.number.
                step = 1;
                previous_block = block.number;
        }

        // Step 2, called by player1 when joining the game
        // NB: the player1 address and amount MUST match, and the hand must be valid.
        function play (uint8 _hand1) external payable
        {
                require(step == 1);
                require_or_set_player1();
                require(msg.value == amount);
                require(_hand1 < 3);

                hand1 = _hand1;

                // Set the step to 2, the block to block.number.
                step = 2;
                previous_block = block.number;
        }

        // Step 3, called by player0 after player1 played, reveals the committed hand.
        function reveal (bytes31 salt, uint8 hand0) external payable {
                require(step == 2);
                require_player0();
                uint8 diff = (hand0 - hand1) % 3;
                if (hand0 >= 3 || player0_commitment != keccak256(abi.encodePacked(salt, hand0))) {
                        player1_wins(2*amount);
                } else if (diff == 1) {
                        player0_wins(2*amount);
                } else if (diff == 2) {
                        player1_wins(2*amount);
                } else {
                        draw(amount);
                }
                step = 0 ;
        }

        // Step 2 bis, rescind the offer to play -- called by player0 after player1 times out.
        function rescind () external payable {
                require(step == 1);
                require_player0();
                require_timeout();
                player0_wins(amount);
                step = 0;
        }

        // Step 3 bis, win by default -- called by player1 after player0 times out rather than reveal hand.
        function win_by_default () external payable {
                require(step == 2);
                require_player1();
                require_timeout();
                player1_wins(amount);
                step = 0;
        }
}

contract RockPaperScissorsFactory
{
        function createRockPaperScissors (bytes32 _commitment, address payable _player1) public payable
        {
                RockPaperScissors c = new RockPaperScissors();
                c.fund(_commitment, _player1);
        }
}
