 

pragma solidity ^0.5.9;
pragma experimental ABIEncoderV2;

contract StateChannelTypes {
    struct ProcessState {
        address processor;  
        bytes32 stateRoot;  
    }

    struct Balances {  
        uint[] owned;  
        uint[] collaterals;  
        uint[] failures;  
        uint[] deadlines;  
             
             
             
    }

    struct BalancedState {
        bytes32 processState;  
        bytes32 balances;  
    }

    struct State {
        bytes32 session;  
        uint64 clock;  
        address[] participants;  
        bytes32 balancedState;  
    }

    enum UnanimousAction {
        Updating,  
        Settling,  
        Closing  
    }
}

contract StateChannelFunctions is StateChannelTypes {

    function digestProcessState(address processor, bytes32 stateRoot)
        internal
        pure
        returns(bytes32 digest)
    {
            return keccak256(abi.encodePacked(processor, stateRoot));
    }

    function digestArray (
            uint[] memory array)
            internal
            pure
            returns(bytes32 digest)
    {
            return keccak256(abi.encodePacked(array));
    }

    function digestBalances (
        bytes32 owned,
        bytes32 collaterals,
        bytes32 failures,
        bytes32 deadlines)
        internal
        pure
        returns(bytes32 digest)
    {
        return keccak256(abi.encodePacked(owned, collaterals, failures, deadlines));
    }

    function digestBalancedState(bytes32 processState, bytes32 balances)
        internal
        pure
        returns(bytes32 digest)
    {
        return keccak256(abi.encodePacked(processState, balances));
    }

    function digestState(
        bytes32 session,
        uint64 clock,
        address payable[] memory participants,
        bytes32 balancedState
    )
        internal
        pure
        returns(bytes32 digest)
    {
        return keccak256(abi.encodePacked(session, UnanimousAction.Updating, clock, participants, balancedState));
    }
}

contract MessageProcessor is StateChannelTypes {
     
     struct ProcessMessageParams {
        bytes32 session;  
        uint64 clock;  
        address payable[] participants;  
        address processor;  
        bytes32 stateRoot;  
        bytes32 balances;  
        bytes message;  
        bytes evidence;  
     }
     function processMessage(
         ProcessMessageParams calldata p)
         external pure returns(bytes32 newBalancedState);
}

contract StateChannelBase is StateChannelTypes, StateChannelFunctions {
     
        function timeoutInBlocks() public pure returns(uint timeout) { return 12 ;}

     
     
     
}

contract StateChannel is StateChannelBase {

     
    bytes32 currentState;

     
    function checkState(
        bytes32 session,
        uint64 clock,
        address payable[] memory participants,
        bytes32 balancedState
    )
        internal
        view
    {
        require(currentState == digestState(session, clock, participants, balancedState));
    }

     
    function checkSignatures(
        bytes32 digest,
        address payable[] memory participants,
        bytes memory signatures_v,
        bytes32[] memory signatures_r,
        bytes32[] memory signatures_s
    )
        public
        pure
    {
        uint len = participants.length;
        uint j = 0;
        for (uint i = 0; i < len; i++) {
            if (participants[i] != address(0)) {
                require(participants[i] ==
                    ecrecover(digest,
                              uint8(signatures_v[j]),
                              signatures_r[j],
                              signatures_s[j]));
                j++;
            }
        }
    }

    function withdraw(
        address payable[] memory participants,
        uint[] memory withdrawals
    )
        internal
    {
        uint len = participants.length;
        for (uint i = 0; i < len; i++) {
            participants[i].transfer(withdrawals[i]);
        }
    }

     
     
    event Unanimously(bytes32);

    struct CloseParams {
        bytes32 session;
        uint64 clock;
        address payable[] participants;
        bytes32 balancedState;
        uint[] withdrawals;
        address payable beneficiary;
        bytes signatures_v;
        bytes32[] signatures_r;
        bytes32[] signatures_s;
    }

    function close(
        CloseParams calldata p)
        external
        payable
    {
        require(msg.value == 0);
        checkState(p.session, p.clock, p.participants, p.balancedState);
        bytes32 digest = keccak256(abi.encodePacked(p.session, UnanimousAction.Closing, p.withdrawals, p.beneficiary));
        checkSignatures(digest, p.participants, p.signatures_v, p.signatures_r, p.signatures_s);
        emit Unanimously(digest);
        withdraw(p.participants, p.withdrawals);
        currentState = 0;
        selfdestruct(p.beneficiary);
    }

    struct SettleParams {
        bytes32 session;
        uint64 clock;
        address payable[] participants;
        bytes32 data;
        uint deposit;
        uint[] withdrawals;
        bytes32 newState;
        bytes signatures_v;
        bytes32[] signatures_r;
        bytes32[] signatures_s;
    }

    function settle(
        SettleParams calldata p)
        external
        payable
    {
        require(msg.value == p.deposit);
        checkState(p.session, p.clock, p.participants, p.data);
        bytes32 digest = keccak256(abi.encodePacked(
                                           p.session, UnanimousAction.Settling,
                                           p.deposit, p.withdrawals, p.newState));
        checkSignatures(digest, p.participants, p.signatures_v, p.signatures_r, p.signatures_s);
        emit Unanimously(digest);
        withdraw(p.participants, p.withdrawals);
        currentState = p.newState;
    }

    event Challenge(uint challengedParticipant);

    struct ChallengeParams {
        bytes32 session;
        uint64 clock;
        address payable[] participants;
        bytes32 processState;
        bytes32 owned;
        bytes32 collaterals;
        bytes32 failures;
        uint[] deadlines;
        uint8 challengingParticipant;
        uint8 challengedParticipant;
    }

    function challenge(
        ChallengeParams calldata p
    )
        external
    {
        require(msg.sender == p.participants[p.challengingParticipant]);
         
         
         
         
        require(p.deadlines[p.challengedParticipant] == 1 ||
                p.challengedParticipant == p.challengingParticipant);
        bytes32 balances = digestBalances(p.owned, p.collaterals, p.failures, digestArray(p.deadlines));
        bytes32 balancedState = digestBalancedState(p.processState, balances);
        checkState(p.session, p.clock, p.participants, balancedState);
        emit Challenge(p.challengedParticipant);
        uint[] memory deadlines = p.deadlines;
        deadlines[p.challengedParticipant] = block.number + timeoutInBlocks();
        bytes32 newBalances = digestBalances(p.owned, p.collaterals, p.failures, digestArray(deadlines));
        bytes32 newBalancedState = digestBalancedState(p.processState, newBalances);
        currentState = digestState(p.session, p.clock, p.participants, newBalancedState);
    }

    struct UpdateStateParams {
        bytes32 session;
        uint64 clock;
        address payable[] participants;
        bytes32 balancedState;
        uint64 newClock;
        bytes32 newBalancedState;
        bytes signatures_v;
        bytes32[] signatures_r;
        bytes32[] signatures_s;
    }

    function updateState(
        UpdateStateParams calldata p)
        external
    {
        checkState(p.session, p.clock, p.participants, p.balancedState);
        require(p.newClock > p.clock);
        bytes32 digest = digestState(p.session, p.newClock, p.participants, p.newBalancedState);
        checkSignatures(digest, p.participants, p.signatures_v, p.signatures_r, p.signatures_s);
        emit Unanimously(digest);
        currentState = digest;
    }

    function nextClock(
        uint64 clock,
        address payable[] memory participants
    )
        public
        pure
        returns(uint64 newClock)
    {
        uint n = participants.length;
        do {
            clock++;
        } while (participants[clock % n] == address(0));
        return clock;
    }

    event TimeOut(uint64 clock, uint failedParticipant);
    struct TimeOutParams
    {
        bytes32 session;
        uint64 clock;
        address payable[] participants;
        bytes32 processState;
        uint[] owned;
        uint[] collaterals;
        uint[] failures;
        uint[] deadlines;
        uint8 failedParticipant;
    }

    function timeOut(
        TimeOutParams calldata p)
        external
        payable
    {
        address payable[] memory participants = p.participants;
        uint[] memory owned = p.owned;
        uint[] memory collaterals = p.collaterals;
        uint[] memory failures = p.failures;
        uint[] memory deadlines = p.deadlines;
        bytes32 balances = digestBalances(
                digestArray(owned),
                digestArray(collaterals),
                digestArray(failures),
                digestArray(deadlines));
        bytes32 balancedState = digestBalancedState(p.processState, balances);
        checkState(p.session, p.clock, p.participants, balancedState);
        uint deadline = deadlines[p.failedParticipant];
        require(deadline > 1 && block.number > deadline);

        address payable failedParticipantAddress = p.participants[p.failedParticipant];
        failedParticipantAddress.transfer(owned[p.failedParticipant]);
        participants[p.failedParticipant] = address(0);
        failures[p.failedParticipant] = uint(failedParticipantAddress);
        owned[p.failedParticipant] = 0;

        uint64 newClock = nextClock(p.clock, participants);
        emit TimeOut(newClock, p.failedParticipant);

        bytes32 newBalances = digestBalances(
            digestArray(owned),
            digestArray(collaterals),
            digestArray(failures),
            digestArray(deadlines));
        bytes32 newBalancedState = digestBalancedState(p.processState, newBalances);
        currentState = digestState(p.session, newClock, participants, newBalancedState);
    }

    event Message(uint64 clock, bytes message);

    struct SendMessageParams
    {
        bytes32 session;
        uint64 clock;
        address payable[] participants;
        address processor;
        bytes32 stateRoot;
        bytes32 balances;
        bytes message;
        bytes evidence;
    }

    function sendMessage(
        SendMessageParams calldata p)
        external
        payable
    {
        address payable[] memory participants = p.participants;
        bytes32 processState = digestProcessState(p.processor, p.stateRoot);
        bytes32 balancedState = digestBalancedState(processState, p.balances);
        checkState(p.session, p.clock, participants, balancedState);

        uint64 new_clock = nextClock(p.clock, participants);
        uint8 participant = uint8(new_clock % participants.length);
        address payable participantAddress = participants[participant];
        require(msg.sender == participantAddress);

        MessageProcessor.ProcessMessageParams memory q =
            MessageProcessor.ProcessMessageParams(
                p.session, new_clock, participants,
                p.processor, p.stateRoot, p.balances, p.message, p.evidence);
        bytes32 newBalancedState = MessageProcessor(p.processor).processMessage(q);

        emit Message(new_clock, p.message);
        currentState = digestState(p.session, new_clock, participants, newBalancedState);
    }

    constructor (bytes32 state) public payable
    {
        emit Unanimously(state);
        currentState = state;
    }
}
