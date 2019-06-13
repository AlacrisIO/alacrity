/* -- This is the contract code body; the initialization code must also set initial data,
   -- which depends on knowing what variables the delegate contract expects where with what initial values.
   -- The cost of the whole thing is of the order of a hundred gas (it increases with the size of the data,
   -- at the rate of 3 GAS per 32-byte word, which is still very cheap,
   -- drowned in the 21000 basic cost of a transaction)
   -- cdsiw: call data size in words
   -- note however, that only 63/64th of the gas is passed to the delegate contract,
   -- increasing the gas cost by about 1.6% (with compound interests for each layer of calling),
   -- though if I read the EIP-150 document correctly and somehow it is more correct than the yellow paper,
   -- maybe not if we compute the gas precisely instead of leave "all the gas that remains"
   -- in the delegatecall gas spec; but the jellopaper seems to confirm the yellowpaper
   -- that this is impossible.
   -- Thus, using delegate is only useful for contracts that have a short-lived interaction,
   -- where the cost of duplicating the entire contract would dominate the cost of those interactions;
   -- but whether it's for a simple game like rock-paper-scissors, for a state channel,
   -- or for a contract-only-invoked-in-case-of-dispute, that will be the case indeed.
  0x58 // PC, -- 0 | cost 2, total 2
  0x81 // DUP1 -- 0 0 | cost 3, total 5
  0x81 // DUP1 -- 0 0 0 | cost 3, total 8
  0x81 // DUP1 -- 0 0 0 0 | cost 3, total 11
  0x36 // CALLDATASIZE -- size 0 0 0 0 | cost 2, total 13
  0x36 // CALLDATASIZE -- size size 0 0 0 0 | cost 2, total 15
  0x82 // DUP3 -- 0 size size 0 0 0 0 | cost 3, total 16
  0x80 // DUP1 -- 0 0 size size 0 0 0 0 | cost 3, total 19
  0x37 // CALLDATACOPY -- size 0 0 0 0 | cost 2 + 3*cdsiw, total 21 + 3*cdsiw
  0x81 // DUP2 -- 0 size 0 0 0 0 | cost 3, total 24 + 3*cdsiw
  0x73 // PUSH20 -- address 0 size 0 0 0 0 | cost 3, total 27 + 3*cdsiw
  XXX -- insert 20-byte delegate address here
  0x61 0x02 0xC9 // PUSH2 713 | cost 3, total 30 + 3*cdsiw
  0x5A // GAS -- gas gas_adjustment address 0 size 0 0 0 0 | cost 2, total 32 + 3*cdsiw
  0x03 // SUB -- adjusted_gas address 0 size 0 0 0 0 | cost 3, total 35 + 3*cdsiw
  0xf4 // DELEGATECALL -- success 0 0 | cost 700, total 735 + 3*cdsiw + gas_spent*64/63
  0x60 0x29 // PUSH1 foo -- label success 0 0 | cost 3, total 738 + 3*cdsiw + gas_spent*64/63
  0x57 // JUMPI -- 0 0 | cost 10, total 748 + 3*cdsiw + gas_spent*64/63
  0xfd // REVERT -- 0 0 | cost 0, total 748 + 3*cdsiw + gas_spent*64/63
foo:
  0xf3 // RETURN -- 0 0 | cost 0, total 748 + 3*cdsiw + gas_spent*64/63
*/

contract Proxy {
function foo () public payable
{
        assembly {
calldatacopy(0, 0, calldatasize)
let result := delegatecall(gas, 0xb0a7600df00dd00d1337deadbeef150123456789, 0, calldatasize, 0, 0)
if result {
return(0,0)
}
revert(0,0)
}
}
}
