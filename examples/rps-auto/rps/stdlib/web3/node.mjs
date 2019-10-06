// vim: filetype=javascript

import Web3            from 'web3';
import * as crypto     from 'crypto';
import * as nodeAssert from 'assert';
import ethers          from 'ethers';

import { mkStdlib } from './common.mjs';

export const stdlibNode = (abi, bytecode, uri, uri_shh) =>
  Promise.resolve(mkStdlib(
    { web3:          new Web3(new Web3.providers.HttpProvider(uri))
    , web3_shh:      new Web3(new Web3.providers.WebsocketProvider(uri_shh))
    , random32Bytes: () => crypto.randomBytes(32)
    , asserter:      nodeAssert.strict
    , abi
    , bytecode
    , ethers
    }));
