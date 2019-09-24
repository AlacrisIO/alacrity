pragma solidity ^0.5.2;



pragma solidity ^0.5.9;

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
        uint clock;
        address[] participants;
        bytes32 balancedState;
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

    function digestBalances (
        uint[] memory owned,
        uint[] memory collaterals,
        uint[] memory failures,
        uint[] memory deadlines)
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
        uint clock,
        address payable[] memory participants,
        bytes32 balancedState
    )
        internal
        pure
        returns(bytes32 digest)
    {
        return keccak256(abi.encodePacked(session, clock, participants, balancedState));
    }
}

contract MessageProcessor is StateChannelTypes {

    function processMessage(
        bytes32 session,
        uint clock,
        address payable[] calldata participants,
        address processor,
        bytes32 stateRoot,
        bytes32 balances,
        bytes calldata message,
        bytes calldata evidence
    ) external pure returns(bytes32 newBalancedState);
}

contract StateChannelBase is StateChannelTypes, StateChannelFunctions {

    function timeoutInBlocks() public pure returns(uint timeout);
}


contract StateChannel is StateChannelBase {

    bytes32 currentState;


    function checkState(
        bytes32 session,
        uint clock,
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

    enum UnanimousAction {
        Updating,
        Settling,
        Closing
    }

    function close(
        bytes32 session,
        uint clock,
        address payable[] calldata participants,
        bytes32 data,
        uint[] calldata withdrawals,
        address payable beneficiary,
        bytes calldata signatures_v,
        bytes32[] calldata signatures_r,
        bytes32[] calldata signatures_s
    )
        external
        payable
    {
        require(msg.value == 0);
        checkState(session, clock, participants, data);
        bytes32 digest = keccak256(abi.encode(session, UnanimousAction.Closing, withdrawals, beneficiary));
        checkSignatures(digest, participants, signatures_v, signatures_r, signatures_s);
        emit Unanimously(digest);
        withdraw(participants, withdrawals);
        currentState = 0;
        selfdestruct(beneficiary);
    }

    function settle(
        bytes32 session,
        uint clock,
        address payable[] calldata participants,
        bytes32 data,
        uint deposit,
        uint[] calldata withdrawals,
        bytes32 newState,
        bytes calldata signatures_v,
        bytes32[] calldata signatures_r,
        bytes32[] calldata signatures_s
    )
        external
        payable
    {
        require(msg.value == deposit);
        checkState(session, clock, participants, data);
        bytes32 digest = keccak256(abi.encode(session, UnanimousAction.Settling, deposit, withdrawals, newState));
        checkSignatures(digest, participants, signatures_v, signatures_r, signatures_s);
        emit Unanimously(digest);
        withdraw(participants, withdrawals);
        currentState = newState;
    }

    event Challenge(uint challengedParticipant);

    function challenge(
        bytes32 session,
        uint clock,
        address payable[] calldata participants,
        bytes32 processState,
        uint[] calldata owned,
        uint[] calldata collaterals,
        uint[] calldata failures,
        uint[] calldata _deadlines,
        uint challengingParticipant,
        uint challengedParticipant
    )
        external
    {
        require(msg.sender == participants[challengingParticipant]);
        require(_deadlines[challengedParticipant] == 1);
        bytes32 balances = digestBalances(owned, collaterals, failures, _deadlines);
        bytes32 balancedState = digestBalancedState(processState, balances);
        checkState(session, clock, participants, balancedState);
        emit Challenge(challengedParticipant);
        uint[] memory deadlines = _deadlines;
        deadlines[challengedParticipant] = block.number + timeoutInBlocks();
        bytes32 newBalances = digestBalances(owned, collaterals, failures, deadlines);
        bytes32 newBalancedState = digestBalancedState(processState, newBalances);
        currentState = digestState(session, clock, participants, newBalancedState);
    }

    function updateState(
        bytes32 session,
        uint clock,
        address payable[] calldata participants,
        bytes32 balancedState,
        uint newClock,
        bytes32 newBalancedState,
        bytes calldata signatures_v,
        bytes32[] calldata signatures_r,
        bytes32[] calldata signatures_s
    )
        external
    {
        checkState(session, clock, participants, balancedState);
        require(newClock > clock);
        bytes32 digest = keccak256(
                abi.encode(session, UnanimousAction.Updating, newClock, newBalancedState));
        checkSignatures(digest, participants, signatures_v, signatures_r, signatures_s);
        emit Unanimously(digest);
        currentState = digestState(session, newClock, participants, newBalancedState);
    }

    function nextClock(uint clock, address payable[] memory participants) public pure returns(uint) {
        uint n = participants.length;
        do {
            clock++;
        } while (participants[clock % n] == address(0));
        return clock;
    }

    event TimeOut(uint clock, uint failedParticipant);

    function timeOut(
        bytes32 session,
        uint clock,
        address payable[] calldata _participants,
        bytes32 processState,
        uint[] calldata _owned,
        uint[] calldata _collaterals,
        uint[] calldata _failures,
        uint[] calldata _deadlines,
        uint failedParticipant
    )
        external payable
    {
        address payable[] memory participants = _participants;
        uint[] memory owned = _owned;
        uint[] memory collaterals = _collaterals;
        uint[] memory failures = _failures;
        uint[] memory deadlines = _deadlines;
        bytes32 balances = digestBalances(owned, collaterals, failures, deadlines);
        bytes32 balancedState = digestBalancedState(processState, balances);
        checkState(session, clock, participants, balancedState);
        uint deadline = deadlines[failedParticipant];
        require(deadline > 1 && block.number > deadline);

        address payable failedParticipantAddress = participants[failedParticipant];
        failedParticipantAddress.transfer(owned[failedParticipant]);
        participants[failedParticipant] = address(0);
        failures[failedParticipant] = uint(failedParticipantAddress);
        owned[failedParticipant] = 0;

        uint newClock = nextClock(clock, participants);
        emit TimeOut(newClock, failedParticipant);

        bytes32 newBalances = digestBalances(_owned, _collaterals, _failures, deadlines);
        bytes32 newBalancedState = digestBalancedState(processState, newBalances);
        currentState = digestState(session, newClock, participants, newBalancedState);
    }

    event Message(uint clock, bytes message);

    function sendMessage(
        bytes32 _session,
        uint _clock,
        address payable[] calldata _participants,
        address _processor,
        bytes32 _stateRoot,
        bytes32 _balances,
        bytes calldata _message,
        bytes calldata _evidence)
        external payable
    {
        address payable[] memory participants = _participants;
        bytes32 processState = digestProcessState(_processor, _stateRoot);
        bytes32 balancedState = digestBalancedState(processState, _balances);
        checkState(_session, _clock, participants, balancedState);

        uint new_clock = nextClock(_clock, participants);
        uint participant = new_clock % _participants.length;
        address payable participantAddress = participants[participant];
        require(msg.sender == participantAddress);

        bytes32 newBalancedState = MessageProcessor(_processor).processMessage(
                _session, new_clock, participants,
                _processor, _stateRoot, _balances, _message, _evidence);

        emit Message(new_clock, _message);
        currentState = digestState(_session, new_clock, participants, newBalancedState);
    }

    constructor (bytes32 state) public payable
    {
        emit Unanimously(state);
        currentState = state;
    }
}
