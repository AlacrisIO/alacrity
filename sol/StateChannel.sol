/**
State Channels

A state channel is a contract guaranteeing the correct behavior of participants
in an interaction where the normal behavior is for participants to
send messages to each other and collect unanimous signatures
about the resulting *states* of the interaction.
But if one of the participants times out during off-chain communications,
the others can get out without his collaboration by posting a state on the contract,
and challenging the stalling participant to either resume participation or
timeout on-chain and forfeit any collateral assets.

As a contract, the channel has the following state variables:
A set of participants p : P, with n = cardinal(p).
Usually, n = 2 and the participants are called Alice and Bob, or A and B.
But when creating the contract, for instance, A is the only member and n=1.
Also, if A (or B) wants to leave but the other wants to stay, we will be back to n=1.
Some contracts may have more than two participants at a time.

Ideally, we could use Schnorr signatures or some similar O(1) scheme for unanimous multisig,
but we still need a simple list-of-signatures scheme as a fallback when a participant times out.
For n=2 especially, it's not worth the trouble of reimplementing crypto primitives,
and list-of-signatures is actually better because of some obvious optimizations.
Actually, for n>2, it might be better if the contract accepts two kinds of signatures,
so participants have a fast path to unanimous confirmation, and a path to adversarial confirmation.

A state message is:
  * session: a nonce, obtained off-chain with the usual shared random number protocol, as session id.
  * clock: a clock number, increasing with every message (or message batch)--sometimes by more than one
    when optional messages are omitted, sometimes by less than one when messages are batched.
  * participants: a commitment to the individual (and, if applicable, combined) public keys that
    identify who can participate in the interaction.
    Starts with cardinality, followed by sequence of keys, and, if present, combined key
  * balancedState: ProcessState plus Balance, where:
    * processState:
      * the address of a MessageProcessor contract, possibly pre-computed from a CREATE2
        formula with NULL sender, so that it contract only has to be instantiated in case of dispute, and
      * a bytes32 piece of data that will typically be the root of a merklized data structure
        for the state of the computation.
    * balances:
      * owned: per-player distribution of assets definitely returned to them even if they timeout
      * collaterals: per-player distribution of assets they'll lose if they timeout but otherwise keep
      * failures: table of failed participants, their index, their address and collateral
      * deadlines: for each player who *must* to publish a message on time, the block number before which
        they must publish, or else be expelled from the contract, losing their collateral
        that is added to the pot (TODO: figure out how the pot is represented).

If all participants agree to end the interaction, they sign message that describes
(1) The final distribution of assets for this nonce-identified interaction.
(2a) a successor interaction, with its new nonce, clock, set of participants and data, OR
(2b) for Ethereum, closing the contract (with given beneficiary address)
When posted on the contract, the effect is immediate.

Off-chain communication happens as follows, in a way reminiscent of token ring networks.
(For two-participant communications, that's just a back-and-forth.)
We assume each participant has an IP address, a tor routing address, an elixxir address,
or is otherwise reachable by other participants.
We assume that there is an agreed-upon order for the participants.
At any moment, one of the participants, determined in a round-robin fashion,
is the proposer who will send a state update based on what the state
received from the previous participant, after checking its validity.
An honest participant will refuse to sign an invalid state,
and instead alert the contract (see below).
If the state is valid, he will sign each of past states he hasn't seen yet,
create a new state with incremented clock and
a list of messages of his own, sign the new state, and pass all that information
to the next person in the ring.
(If there are more than two participants, maybe also send the information
in the other direction, to speed up recovery
in the case of a single participant failure.)

If the protocol reaches a point where all messages are optional, and
no message was sent in the round, then the current proposer becomes a listener
for the next round: he waits for himself or someone else to send a new message;
anyone can initiate a new message and send it to the proposer,
with a clock number that skips other participants before them in the round.
The proposer signs his update with at most one other such participant,
plus his own optional message, and the clock that skips the rest of the round,
then a new round is started.

The proposer is identified by the clock modulo the number of participants;
if the number of participants changes, a new interaction nonce is produced,
and the list of participants is updated so the first participant in the new interaction
is the next one in the current interaction contract a few clock ticks may be skipped
so the next in line is still determined as per the round-robin algorithm.
If some participant signs two conflicting state updates with the same clock but different contents,
or times out when it's their turn to post, they are considered failing.

If some participant considers the system to have timed out,
he tries to contact other participants directly to debug the situation:
determine the latest known state signed by everyone,
who is therefore probably blocking the situation,
and issue a challenge on the contract by publishing that latest known state
and demanding that they keep the ball rolling.
In the case of optional messages not being relayed by the proposer,
one of the message issuers posts the listening state plus their message
then demands that the proposer shall continue.

A challenged proposer may simply post an update signed by everyone
with a clock larger than the current one, and this cancels the challenge.
This state update may or may not be accompanied by a further challenge
to the same or another proposer.
If some others refuse to sign the proposed state, then the proposer may instead
post the new state, signed by him only, together with a proof
that the old state leads to the new state through a message they send:
typically, the list of actual messages plus any ancillary data
to help the proof check (revealing the relevant parts of the merklized state).
The proof is checked by the contract.

In case computations are expensive, only posting the data may be required
unless there is a subsequent challenge by a participant;
the balances would only be updated after the computation is validated;
this could be done by another layer on top of state channels —
actually, a transformation applied before the state channel transformation.
Similarly, if posting data is expensive, this challenge would only include
posting a proof of publication to the MKB.

If a challenged proposer fails to post a valid update,
they are evicted from the interaction; the contract will send them back
whatever assets they fully own, but they will forfeit their collateral
(the two are stored in a table that is part of the signed state).
The collateral is divided between other players according to a formula
that the next proposer has to apply when we proposes his state.
(Typically, the collateral is uniformly divided evenly between participants,
plus an extra part sent to whoever paid to prove the proposer failing,
to cover their gas; or in some games or in two-person interactions,
it may be added to a pool that will go to the final winner;
all correct formulas have the same outcome for two-participant interactions).

To leave an interaction cooperatively, sign a settlement.
To leave an interaction without cooperation from others
(who refuse to make progress to a safe point then sign a settlement),
then challenge everyone to make progress to that safe point,
until you can challenge yourself and exit with your principal,
hopefully after having reduced your collateral to zero (or as little as possible)
via the progress protocol.

Optionally, for extra punishment, a participant may also issue a challenge
that a player signed an invalid state:
exhibit two states with the same signature, or a signed state
and an accepted state that differ for the same number.
Thus, if you detect an invalid state, you publish the latest signed valid state,
force the publication of the invalid data you saw: either they publish the same data,
and fail validation since it's invalid, and/or possibly timeout,
or they publish different data and get punished.


Adding assets and/or participants is done through settlement.
The settlement handler takes as parameter of the settlement an amount of tokens
that some participant must deposit as a condition for the new settlement to be valid.
To use this strategy, first, participants sign a settlement corresponding to
the current state of the interaction; they keep that settlement as fallback
in case the prospective new entrant fails to post his transaction.
If one participant refuses to cooperate, other participants exit with the last state and messages.
Then, participants sign a settlement corresponding to the entry of the new participant.
The new participant can sign the agreement and post it with the required amount of money,
until one of the previous participants grows tired (and/or a timeout happens)
and posts the fallback transaction.
TODO: Probably, settlements should have add timeins and timeouts too.
Probably no need for too much additional logic in the settlement function,
since logic can be implemented through messages.
TODO: Also, in the future, a protocol to specially accept or disburse ERC20 / ERC777 tokens
or ERC721 NFTs.

Because each and every participant can stall the interaction,
state channels don't scale to more participants than you'd care to bear
the stalling from. For instance, with a two-day timeout, two rounds
to get out of the interaction, and five participants,
that's four participants who may sequentially timeout for you twice two days,
so sixteen days of waiting to get your assets out if they collude against you.
Can you wait that long? Moreover, in any game with more than two participants
where collusion brings any advantage, whether by sharing secret information
or by ganging up on a player, attackers will naturally collude to win,
therefore it mightn't be safe to play those games on the blockchain.
*/

pragma solidity ^0.5.2;

contract StateChannelTypes {
    struct ProcessState {
        address processor; // message processor contract
        bytes32 stateRoot; // root of merklized data describing the state of the interaction.
    }

    struct Balances { // TODO: should each of these data items be salted and merklized?
        uint[] owned; // per-player distribution of assets definitely owned even if they timeout
        uint[] collaterals; // per-player distribution of assets they'll lose if they timeout.
        uint[] failures; // list of failed participants, their former index, address and collateral
        uint[] deadlines; // per-player deadlines for when the player must post his next message or else
            // deadline 0 means the player doesn't have to post a message at this point
            // deadline 1 means the player must post a message at this point, but hasn't been challenged
            // deadline N>1 means the player has been challenged and must post a message before block N
    }

    struct BalancedState {
        bytes32 processState; // merklized continuation
        bytes32 balances; // merklized assets structure
    }

    struct State {
        bytes32 session; // a session nonce, obtained with the usual shared random number protocol offchain
        uint clock; // a clock number, increasing at every step in the token ring
        address[] participants; // also have a Schnorr public key? Make it a fixed-size array? a pair?
        bytes32 balancedState; // digest of balancedState
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
    /** Main function that must be provided: process a message.
        This must be a pure function, that as input takes
        The message data is what gets logged in an event,
        and MUST include all new information.
        The ancillary data it accepts MUST be comprised only
        of data that is already know to every participant because they either signed it
        or saw it as data logged in Message events on the contract.
        The ancillary data probably includes
        decoding for data as a BalancedState data structure, as well as further
        merkle witnesses to decode relevant parts of the state, etc.
        TODO: make that a call to a different contract, which is more expensive, but that's OK
        since it's not supposed to be used in normal case.
     */
    function processMessage(
        bytes32 session, // Mutually-generated random nonce that identifies the StateChannel session.
        uint clock, // serial number for the off-chain state within the session.
        address payable[] calldata participants, // the list of current participants in the state channel.
        address processor, // address of the MessageProcessor contract
        bytes32 stateRoot, // root of merklized data describing the state of the interaction.
        bytes32 balances, // the digest for the balancedState
        bytes calldata message, // new data, that will be logged as event
        bytes calldata evidence // old data that won't be logged because users already know it, but that will be used to convince the judge that the transition is correct. Typically, it consists in revealing a subgraph of the merklized balancedState.
    ) external pure returns(bytes32 newBalancedState);
}

contract StateChannelBase is StateChannelTypes, StateChannelFunctions {
    // This function returns the number of blocks before a timeout
    function timeoutInBlocks() public pure returns(uint)
    {
      return 42;
    }
}


contract StateChannel is StateChannelBase {

    bytes32 currentState;

    /** Check that the previous state is what we believe it is. */
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

    /** Check that the signatures match the digested data.
        Skip participants who have been evicted for timeout.
     */
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

    // Participants already have all the data to interpret the meaning of a unanimous action,
    // so we don't need to reveal more than the digest of that action.
    event Unanimously(bytes32);

    enum UnanimousAction {
        Updating, // update the state of the channel
        Settling, // settle the channel
        Closing // close the channel
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
/*
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
*/
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
/*
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

        bytes32 newBalances = digestBalances(owned, _collaterals, failures, deadlines);
        bytes32 newBalancedState = digestBalancedState(processState, newBalances);
        currentState = digestState(session, newClock, participants, newBalancedState);
    }
*/
    event Message(uint clock, bytes message);
/*
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
*/
    constructor (bytes32 state) public payable
    {
        emit Unanimously(state);
        currentState = state;
    }
}
