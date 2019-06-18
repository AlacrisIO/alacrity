export const contractAbi = [{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v14","type":"uint256"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"}],"name":"msg0_m","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v14","type":"uint256"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"},{"name":"v23","type":"uint256"}],"name":"msg1_m","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v14","type":"uint256"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"},{"name":"v23","type":"uint256"},{"name":"v33","type":"uint256"},{"name":"v34","type":"uint256"}],"name":"msg2_m","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"}],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v14","type":"uint256"},{"indexed":false,"name":"v15","type":"uint256"},{"indexed":false,"name":"v16","type":"uint256"}],"name":"msg0_evt","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v23","type":"uint256"}],"name":"msg1_evt","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v33","type":"uint256"},{"indexed":false,"name":"v34","type":"uint256"}],"name":"msg2_evt","type":"event"}];

export const contractFactoryAbi = [{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v14","type":"uint256"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"}],"name":"make","outputs":[{"name":"_ctc","type":"address"}],"payable":true,"stateMutability":"payable","type":"function"}];

export const contractFactoryCode = "0x608060405234801561001057600080fd5b50610fa0806100206000396000f3fe60806040526004361061001e5760003560e01c8063f346534214610023575b600080fd5b6100a3600480360360a081101561003957600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff1690602001909291908035906020019092919080359060200190929190803590602001909291905050506100e5565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b60008086866040516100f69061026f565b808373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200192505050604051809103906000f08015801561017b573d6000803e3d6000fd5b5090508073ffffffffffffffffffffffffffffffffffffffff1663236ba4413489898989896040518763ffffffff1660e01b8152600401808673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001848152602001838152602001828152602001955050505050506000604051808303818588803b15801561024957600080fd5b505af115801561025d573d6000803e3d6000fd5b50505050508091505095945050505050565b610cef8061027d8339019056fe6080604052604051610cef380380610cef8339818101604052604081101561002657600080fd5b81019080805190602001909291908051906020019092919050505060008282604051602001808460ff1681526020018373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200193505050506040516020818303038152906040528051906020012060001c6000819055505050610c01806100ee6000396000f3fe6080604052600436106100345760003560e01c8063236ba44114610039578063352405e7146100bb578063f376748514610147575b600080fd5b6100b9600480360360a081101561004f57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff1690602001909291908035906020019092919080359060200190929190803590602001909291905050506101e8565b005b610145600480360360c08110156100d157600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803590602001909291908035906020019092919080359060200190929190803590602001909291905050506103c5565b005b6101e6600480360361010081101561015e57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190803590602001909291908035906020019092919080359060200190929190803590602001909291908035906020019092919050505061060a565b005b60008585604051602001808460ff1681526020018373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200193505050506040516020818303038152906040528051906020012060001c6000541461028b57600080fd5b8473ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16146102c357600080fd5b7f6b4a32dc08985467ccf8620d8e2a4ff1a90dabbdc8bf8951a60c6c0456bfd9bd83838360405180848152602001838152602001828152602001935050505060405180910390a160018585858585604051602001808760ff1681526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200184815260200183815260200182815260200196505050505050506040516020818303038152906040528051906020012060001c6000819055505050505050565b60018686868686604051602001808760ff1681526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200184815260200183815260200182815260200196505050505050506040516020818303038152906040528051906020012060001c6000541461048057600080fd5b8473ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16146104b857600080fd5b60016104c55760016104c8565b60005b6104d35760016104d6565b60005b61050357600081146104fb57600181146104f357600281146104f6565b60015b6104fe565b60015b610506565b60015b61050f57600080fd5b7f9afa34bc82937d1b0a533cb8e2a5e15afff1aa50035fe490c93b3e6f71c19af0816040518082815260200191505060405180910390a16002868686868686604051602001808860ff1681526020018773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018581526020018481526020018381526020018281526020019750505050505050506040516020818303038152906040528051906020012060001c600081905550505050505050565b6002888888888888604051602001808860ff1681526020018773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018581526020018481526020018381526020018281526020019750505050505050506040516020818303038152906040528051906020012060001c600054146106cd57600080fd5b8773ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff161461070557600080fd5b81604051602001808281526020019150506040516020818303038152906040528160405160200180828152602001915050604051602081830303815290604052604051602001808060200180602001838103835285818151815260200191508051906020019080838360005b8381101561078c578082015181840152602081019050610771565b50505050905090810190601f1680156107b95780820380516001836020036101000a031916815260200191505b50838103825284818151815260200191508051906020019080838360005b838110156107f25780820151818401526020810190506107d7565b50505050905090810190601f16801561081f5780820380516001836020036101000a031916815260200191505b509450505050506040516020818303038152906040526040516020018080602001828103825283818151815260200191508051906020019080838360005b8381101561087857808201518184015260208101905061085d565b50505050905090810190601f1680156108a55780820380516001836020036101000a031916815260200191505b50925050506040516020818303038152906040528051906020012060001c84146108ce57600080fd5b60016108db5760016108de565b60005b6108e95760016108ec565b60005b61091957600081146109115760018114610909576002811461090c565b60015b610914565b60015b61091c565b60015b61092557600080fd5b6000808214610947576001821461093f5760028214610942565b60015b61094a565b60015b9050600080851461096e57600185146109665760028514610969565b60015b610971565b60015b9050600082610981576000610983565b815b6109ab57826109a0578161099857600161099b565b60005b6109a3565b60025b60ff166109bc565b6003866004038501816109ba57fe5b065b9050600081146109df57600181146109d757600281146109da565b60015b6109e2565b60015b6109eb57600080fd5b600281146109fa5760016109fd565b60005b610a2a5760008414610a225760018414610a1a5760028414610a1d565b60015b610a25565b60015b610a2d565b60015b610a3657600080fd5b60008114610a45576001610a48565b60005b610a755760008614610a6d5760018614610a655760028614610a68565b60015b610a70565b60015b610a78565b60015b610a8157600080fd5b6000600282149050600080831490508c73ffffffffffffffffffffffffffffffffffffffff166108fc83610ac35782610abc578b8d01610abe565b8b5b610aca565b8b8d600202015b9081150290604051600060405180830381858888f19350505050158015610af5573d6000803e3d6000fd5b508b73ffffffffffffffffffffffffffffffffffffffff166108fc83610b2a5782610b20578c610b25565b8c6002025b610b2d565b60005b9081150290604051600060405180830381858888f19350505050158015610b58573d6000803e3d6000fd5b507f29e09ade2aca108b41dced0ae0a0bccb3677133e28b2c5253c2977aeeaaf2e5e8787604051808381526020018281526020019250505060405180910390a1600080819055507302b463784bc1a49f1647b47a19452ac420dfc65a73ffffffffffffffffffffffffffffffffffffffff16fffea265627a7a72305820663159957181a68eb12067aaed6f1b3ca0eb81e35332060a85df9bcdc969abe964736f6c63430005090032a265627a7a723058203faafd72cd04e7f985bc2013660b6939d934d4b6306bba038bf1a409fb109c3664736f6c63430005090032";