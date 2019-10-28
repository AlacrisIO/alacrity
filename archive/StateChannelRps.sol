pragma solidity ^0.5.9;
pragma experimental ABIEncoderV2;

import "StateChannel.sol";

contract StateChannelRps is MessageProcessor, StateChannelFunctions {

  struct Header {
          uint32 tag;
  }

  struct State0 {
          uint32 tag; // must be 0
  }

  // No need for an on-chain message to lead to State1:
  // the state is reached through off-chain signing of the preamble of a new game.
  // If Bob declines the game, it will just never reach an on-chain state.
  struct State1 {
          uint32 tag; // must be 1
          uint8 alice;
          uint8 bob;
          uint256 wagerAmount;
          uint256 escrowAmount;
          uint256 commitment; // v13
  }

  struct State2 {
          uint32 tag; // must be 2
          uint8 alice;
          uint8 bob;
          uint256 wagerAmount;
          uint256 escrowAmount;
          uint256 commitment;
          uint256 hand1; // Bob's hand
  }

  struct Message1 {
          uint32 tag; // must be 1
          uint256 hand1;
  }

  struct Evidence1 {
          State1 state;
          uint256[] owned;
          uint256[] collaterals;
          bytes32 failures;
          uint256[] deadlines;
  }

  struct Message2 {
          uint32 tag; // must be 2
          uint256 salt;
          uint256 hand0;
  }

  struct Evidence2 {
          State2 state;
          uint256[] owned;
          uint256[] collaterals;
          bytes32 failures;
          uint256[] deadlines;
  }

  function processMessage(
    ProcessMessageParams calldata p)
      external pure returns(bytes32 newBalancedState) {
          Header memory h = abi.decode(p.message, (Header));
        if (h.tag == 1) {
          Message1 memory m = abi.decode(p.message, (Message1));
          Evidence1 memory e = abi.decode(p.evidence, (Evidence1));
          require(p.stateRoot == keccak256(abi.encode(e.state)));
          require(p.balances == digestBalances(keccak256(abi.encodePacked(e.owned)), keccak256(abi.encodePacked(e.collaterals)), e.failures, keccak256(abi.encodePacked(e.deadlines))));
          require(e.state.tag == 1);
          require(p.participant == e.state.bob);
          require(m.hand1 < 3);
          // Adjust the balances for Bob
          // assert(p.balances[participant] >= e.state.wagerAmount + e.state.escrowAmount); // check it BEFORE the initial message is signed, drop the assertion in the (to be generated) solidity code.
          e.owned[p.participant] -= e.state.wagerAmount + e.state.escrowAmount;
          e.collaterals[p.participant] += e.state.escrowAmount;
          // Set the deadlines
          e.deadlines[p.participant] = 0;
          e.deadlines[e.state.alice] = 1;
          // Now, reconstruct the state from these elements
          return digestBalancedState(
                   digestProcessState(
                     p.processor,
                     keccak256(abi.encode(State2(2,
                                                 e.state.alice,
                                                 e.state.bob,
                                                 e.state.wagerAmount,
                                                 e.state.escrowAmount,
                                                 e.state.commitment,
                                                 m.hand1)))),
                   digestBalances(
                           keccak256(abi.encodePacked(e.owned)),
                           keccak256(abi.encodePacked(e.collaterals)),
                           e.failures,
                           keccak256(abi.encodePacked(e.deadlines))));
        } else if (h.tag == 2) {
          Message2 memory m = abi.decode(p.message, (Message2));
          Evidence2 memory e = abi.decode(p.evidence, (Evidence2));
          require(p.stateRoot == keccak256(abi.encode(e.state)));
          require(p.balances == digestBalances(keccak256(abi.encodePacked(e.owned)), keccak256(abi.encodePacked(e.collaterals)), e.failures, keccak256(abi.encodePacked(e.deadlines))));
          require(e.state.tag == 2);
          require(p.participant == e.state.alice);
          require(e.state.commitment == uint256(keccak256(abi.encodePacked(m.salt, m.hand0))));
          require(m.hand0 < 3);
          // Adjust the balances for Alice and Bob
          uint256 outcome = (m.hand0 + (4 - e.state.hand1)) % 3;
          (uint256 aGets, uint256 bGets) =
                  (outcome == 2) ? (2 * e.state.wagerAmount, 0) :
                  (outcome == 0) ? (0, 2 * e.state.wagerAmount) :
                  (e.state.wagerAmount, e.state.wagerAmount);
          // update balances
          e.owned[p.participant] += aGets + e.state.escrowAmount;
          e.collaterals[p.participant] -= e.state.escrowAmount;
          e.owned[e.state.bob] += bGets + e.state.escrowAmount;
          e.collaterals[e.state.bob] -= e.state.escrowAmount;
          // Set the deadlines
          e.deadlines[p.participant] = 0;
          // Now, reconstruct the state from these elements
          return digestBalancedState(
                   digestProcessState(
                     p.processor,
                     keccak256(abi.encode(State0(0)))),
                   digestBalances(
                           keccak256(abi.encodePacked(e.owned)),
                           keccak256(abi.encodePacked(e.collaterals)),
                           e.failures,
                           keccak256(abi.encodePacked(e.deadlines))));
        } else {
          revert();
        }
  }
}
