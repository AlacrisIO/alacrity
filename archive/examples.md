# Examples

* [nginx](nginx): a simple configuration for running nginx on your localhost,
  such that after you run the [./start.sh](nginx/start.sh) script,
  you can point your browser at < http://localhost:8088/rps-demo/ >
  and play our "Rock, Paper, Scissors" demo.
  To stop the server, run the [./stop.sh](nginx/stop.sh).

* [rock_papers_scissors.ala](rock_papers_scissors.ala):
  a very simple two-person game, to illustrate our conflict resolution mechanisms
  on the simplest adversarial system possible.

* payments.ala:
  Start with phone-to-phone payments, then add some Point-of-Sale capability.
  Add ERC-20 support instead of just ETH tokens.
  See also [ERC777-K](https://runtimeverification.com/blog/erc777-k-formal-executable-specification-of-erc777/).

* side_chain.ala:
  Make transactions fast with a market of side-chain operators kept honest by the MKB.
  Allow for variant where the MKB does all the checking, in the style of the Blockstream Liquid network.
  Allow for censorship-resistant side-chains with higher-latency initially blinded blocks.
  Maybe have a separate file for each variant?

* nft.ala: a Non-Fungible Token in the style of ERC-721, or cryptoscalies,
  video-game tokens, foo-on-the-blockchain.
  Include atomic swap capability so contracts can be written that atomically sell a NFT for ETH, etc.

* poker.ala: some more complex card game where each party can only see part of the state,
  which includes logical constraints.
  See the [ROYALE paper](https://iohk.io/research/papers/#MPEKMMQP) by IOHK.

* escrow.ala: two-out-of-three contract, e.g. for a trusted intermediary to arbitrate disputes over
  two people, or validate the casual transactions of one party that keeps a second key in cold storage.

* crowdfunding.ala: participants pledge money;
  the total amount is disbursed to the designated campaign recipient
  if and only if the required amount is reached before the deadline;
  otherwise it is returned to pledgers.
  This benchmark allows to compare our system to Scilla and Plutus.

* exchange.ala: a non-custodial decentralized exchange.
  Matching partners via an order book, who are then bound to complete an atomic swap
  between the two matched sets of assets.
  (First target Nervos and Ethereum?)

* private_transactions.ala: ZCash or MimbleWimble on a side-chain.

* supply_chain.ala: public acknowledgements for fixed multiparty workflow.

* insurance.ala: futures contract assuming an oracle for e.g. weather or earthquakes.

* automated_bill_payment.ala: subscription to a service with a service meter, and
  an arbiter in case of timely disputes.
