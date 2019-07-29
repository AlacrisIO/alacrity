/**
State Channels

A state channel is a contract guaranteeing the correct behavior of participants
in an interaction where the normal behavior is for participants to
send messages to each other and collect unanimous signatures
about the resulting *states* of the interaction.
But if one of them times out, the others can get out without his collaboration
by posting a state on the contract, and challenging the other party to either
resume participation or timeout and forfeit any contested assets.

As a contract, the channel has the following state variables:
A set of participants p : P, with n = cardinal(p).
Usually, n = 2 and the participants are called Alice and Bob, or A and B.
But when creating the contract, for instance, A is the only member and n=1.
Also, if A (or B) wants to leave but the other wants to stay, we will be back to n=1.

Ideally, we could use Schnorr signatures or some similar O(1) scheme for multisig,
but any stupid old list-of-signatures scheme is fine if that's what the network provides;
for n=2 especially it's not worth the trouble of reimplementing crypto primitives,
and list-of-signatures is actually better because of some obvious optimizations.
Actually, for n>2, it might be better if the contract accepts two kinds of signatures,
so participants have a fast path to confirmation, and a path to cheap confirmation.

A state message is:
  * nonce: a contract nonce, obtained with the usual shared random number protocol.
  * clock: a clock number, increasing with every batch of message--sometimes by more than one
    when optional messages are omitted, sometimes by less than one when messages are batched.
  * participants: a commitment to the individual (and, if applicable, combined) public keys that
    identify who can participate in the interaction.
    Starts with cardinality, followed by sequence of keys, and, if present, combined key
  * balancedContinuation: continuation plus balance, made of:
    * continuation: the continuation: describes how to process a message
    * owned: per-player distribution of assets definitely returned to them even if they timeout
    * collaterals: per-player distribution of assets they'll lose if they timeout.

If all participants agree to end the interaction, they sign message that describes
(1) The final distribution of assets for this nonce-identified interaction.
(2a) a successor interaction, with its new nonce, clock, set of participants and data, OR
(2b) for Ethereum, closing the contract (with given beneficiary address)
When posted on the contract, the effect is immediate.

Off-chain communication happens as follows, in a way reminiscent of token ring networks.
(For two-participant communications, that's just a back-and-forth.)
We assume each participant has an IP address, tor routing address
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
for the next round: he sends that state around for everyone to sign,
and waits for himself or someone else to send a new message;
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
If others refuse to sign the proposed state, then the proposer may instead
post the new state, signed by them only, together with a proof
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


Adding assets and/or participants is problematic, because in a state-channel,
the state is off-chain, and thus cannot be updated by the contract as it receives
those assets. There are two solutions, synchronous and asynchronous:
1- In the synchronous solution, the active participants make a settlement
   to set the contract in a direct style where the contract manages the state;
   then the new participant (or existing participant adding assets)
   can join in a way directly handled by the contract,
   after which the contract can go back in state-channel mode.
   Other attempts to add assets or join the interaction are rejected.
2- In the asynchronous solution, the new participants (or existing participants
   adding assets) will send arbitrary assets registered in a table as in an ERC20,
   where they are locked for some time (to prevent race conditions between
   settlement and withdrawal).
   Then, if all participants *and* the new participants sign a settlement,
   the new participant can be added an the deposit balance is cleared.
   In case of timeout or denial of settlement,
   the new person can withdraw what they deposited.
   It's not clear that this is better than the above.
   Participants could "simply" sign a settlement, and have the depositor
   publish the settlement together with their deposit
   and the deposit-conditional agreement to go back to state-channel mode.
   If the depositor times out, they publish an alternate settlement.

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

pragma solidity ^0.5.9;

contract StateChannel {

    /** First of two functions that must be provided: return the timeout duration in blocks */
    function timeoutInBlocks() public pure returns(uint);

    /** Second of two functions that must be provided: process a message.
        This must be a pure function.
        The message data is what gets logged in an event,
        and MUST include all new information.
        The ancillary data it accepts MUST be comprised only
        of data that is already know to every participant because they either signed it
        or saw it as data logged in Message events on the contract.
        The ancillary data probably includes
        decoding for data as a BalancedData data structure, as well as further
        merkle witnesses to decode relevant parts of the state, etc.
     */
    function processMessage(
        bytes32 nonce,
        uint clock,
        address payable[] memory participants,
        bytes32 data,
        bytes memory message,
        bytes memory ancillaryData
    ) public pure returns(bytes32);

    struct BalancedData {
        bytes32 state ; // merklized data describing the state of the interaction.
        uint[] owned; // per-player distribution of assets definitely owned even if they timeout
        uint[] collaterals; // per-player distribution of assets they'll lose if they timeout.
        uint[] failures; // list of failed participants, their former index, address and collateral
    }

    struct State {
        uint256 nonce; // a contract nonce, obtained with the usual shared random number protocol offchain
        uint clock; // a clock number, increasing at every step in the token ring
        address[] participants; // also have a Schnorr public key? Make it a fixed-size array? a pair?
        bytes32 balancedData; // digest of balancedData
    }

    enum StateAction {
        Updating, // update the state of the channel
        Settling, // settle the channel
        Closing // close the channel
    }

    bytes32 currentState;

    /** Create the contract from the state.
        Be sure to only partake if you know the preimage of the digest,
        and have checked that the contract creation transaction matches that preimage. */
    constructor(bytes32 state) public payable {
        currentState = state;
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

    function digestState(
        bytes32 nonce,
        uint clock,
        address payable[] memory participants,
        bytes32 data,
        uint timeout
    )
        internal
        pure
        returns(bytes32 digest)
    {
        return keccak256(abi.encode(nonce, clock, participants, data, timeout));
    }

    /** Check that the previous state is what we believe it is. */
    function checkState(
        bytes32 nonce,
        uint clock,
        address payable[] memory participants,
        bytes32 data,
        uint timeout
    )
        internal
        view
    {
        require(currentState == digestState(nonce, clock, participants, data, timeout));
    }

    function distribute(
        address payable[] memory participants,
        uint[] memory distribution
    )
        internal
    {
        uint len = participants.length;
        for (uint i = 0; i < len; i++) {
            participants[i].transfer(distribution[i]);
        }
    }

    event Unanymously(bytes32);

    function close(
        bytes32 nonce,
        uint clock,
        address payable[] calldata participants,
        bytes32 data,
        uint timeout,
        uint[] calldata distribution,
        address payable beneficiary,
        bytes calldata signatures_v,
        bytes32[] calldata signatures_r,
        bytes32[] calldata signatures_s
    )
        external
        payable
    {
        checkState(nonce, clock, participants, data, timeout);
        bytes32 digest = keccak256(abi.encode(nonce, StateAction.Closing, distribution, beneficiary));
        checkSignatures(digest, participants, signatures_v, signatures_r, signatures_s);
        emit Unanymously(digest);
        distribute(participants, distribution);
        currentState = 0;
        selfdestruct(beneficiary);
    }

    function settle(
        bytes32 nonce,
        uint clock,
        address payable[] calldata participants,
        bytes32 data,
        uint timeout,
        uint[] calldata distribution,
        bytes32 newState,
        bytes calldata signatures_v,
        bytes32[] calldata signatures_r,
        bytes32[] calldata signatures_s
    )
        external
        payable
    {
        checkState(nonce, clock, participants, data, timeout);
        bytes32 digest = keccak256(abi.encode(nonce, StateAction.Settling, distribution, newState));
        checkSignatures(digest, participants, signatures_v, signatures_r, signatures_s);
        emit Unanymously(digest);
        distribute(participants, distribution);
        currentState = newState;
    }

    event Challenge();

    function challenge(
        bytes32 nonce,
        uint clock,
        address payable[] calldata participants,
        bytes32 data,
        uint timeout,
        uint participant
    )
        external
    {
        require(msg.sender == participants[participant]);
        require(timeout == 0);
        checkState(nonce, clock, participants, data, timeout);
        emit Challenge();
        currentState = digestState(nonce, clock, participants, data, block.number + timeoutInBlocks());
    }

    function update(
        bytes32 nonce,
        uint clock,
        uint timeout,
        address payable[] calldata participants,
        bytes32 data,
        uint new_clock,
        bytes32 new_data,
        bytes calldata signatures_v,
        bytes32[] calldata signatures_r,
        bytes32[] calldata signatures_s
    )
        external
    {
        checkState(nonce, clock, participants, data, timeout);
        require(new_clock > clock);
        bytes32 digest = keccak256(abi.encode(nonce, StateAction.Updating, new_clock, new_data));
        checkSignatures(digest, participants, signatures_v, signatures_r, signatures_s);
        emit Unanymously(digest);
        currentState = digestState(nonce, new_clock, participants, new_data, uint(0));
    }

    function nextClock(uint clock, address payable[] memory participants) public pure returns(uint) {
        uint n = participants.length;
        do {
            clock++;
        } while (participants[clock % n] == address(0));
        return clock;
    }

    function digestData(
        bytes32 state,
        uint[] memory owned,
        uint[] memory collaterals,
        address[] memory failures
    )
        internal
        pure
        returns(bytes32 digest)
    {
        return keccak256(abi.encode(state,
                                    keccak256(abi.encode(owned)),
                                    keccak256(abi.encode(collaterals)),
                                    keccak256(abi.encode(failures))));
    }

    event TimeOut(uint clock, address participant);

    function timeOut(
        bytes32 nonce,
        uint clock,
        address payable[] calldata _participants,
        bytes32 state,
        uint[] calldata _owned,
        uint[] calldata _collaterals,
        address[] calldata _failures,
        uint timeout
    )
        external payable
    {
        address payable[] memory participants = _participants;
        uint[] memory owned = _owned;
        uint[] memory collaterals = _collaterals;
        address[] memory failures = _failures;
        bytes32 data = digestData(state, owned, collaterals, failures);
        checkState(nonce, clock, participants, data, timeout);
        require(block.number > timeout);

        uint new_clock = nextClock(clock, participants);
        uint failedParticipant = new_clock % participants.length;
        address payable failedParticipantAddress = participants[failedParticipant];
        emit TimeOut(new_clock, failedParticipantAddress);

        failedParticipantAddress.transfer(owned[failedParticipant]);
        failures[failedParticipant] = failedParticipantAddress;
        participants[failedParticipant] = address(0);
        owned[failedParticipant] = 0;
        data = digestData(state, owned, collaterals, failures);
        currentState = digestState(nonce, new_clock, participants, data, uint(0));
    }

    event Message(uint clock, bytes messageData);

    function message(
        bytes32 nonce,
        uint clock,
        address payable[] calldata _participants,
        bytes32 data,
        uint timeout,
        bytes calldata messageData,
        bytes calldata ancillaryData
    )
        external payable
    {
        address payable[] memory participants = _participants;
        checkState(nonce, clock, participants, data, timeout);

        uint new_clock = nextClock(clock, participants);
        uint participant = new_clock % participants.length;
        address payable participantAddress = participants[participant];
        require(msg.sender == participantAddress);

        bytes32 new_data = processMessage(nonce, new_clock, participants, data,
                                          messageData, ancillaryData);

        emit Message(new_clock, messageData);
        currentState = digestState(nonce, new_clock, participants, new_data, uint(0));
    }
}
