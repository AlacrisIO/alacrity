export const contractAbi = [{"constant":true,"inputs":[{"name":"c","type":"bytes"}],"name":"ALA_BCAT_RIGHT","outputs":[{"name":"","type":"bytes"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":true,"inputs":[{"name":"l","type":"bytes"},{"name":"r","type":"bytes"}],"name":"ALA_BCAT","outputs":[{"name":"","type":"bytes"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v19","type":"uint256"},{"name":"v20","type":"uint256"},{"name":"v21","type":"uint256"}],"name":"m0","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":true,"inputs":[{"name":"_in","type":"bytes"},{"name":"_start","type":"uint16"},{"name":"_len","type":"uint16"}],"name":"ALA_BYTES_SLICE","outputs":[{"name":"","type":"bytes"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":true,"inputs":[{"name":"c","type":"bytes"}],"name":"ALA_BCAT_LEFT_LENGTH","outputs":[{"name":"","type":"uint16"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v19","type":"uint256"},{"name":"v20","type":"uint256"},{"name":"v21","type":"uint256"},{"name":"v26","type":"uint256"},{"name":"v34","type":"uint256"},{"name":"v35","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":true,"inputs":[{"name":"x","type":"uint256"}],"name":"ALA_INT_TO_BYTES","outputs":[{"name":"","type":"bytes"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v19","type":"uint256"},{"name":"v20","type":"uint256"},{"name":"v21","type":"uint256"},{"name":"v26","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":true,"inputs":[{"name":"c","type":"bytes"}],"name":"ALA_BCAT_LEFT","outputs":[{"name":"","type":"bytes"}],"payable":false,"stateMutability":"pure","type":"function"},{"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"}],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v19","type":"uint256"},{"indexed":false,"name":"v20","type":"uint256"},{"indexed":false,"name":"v21","type":"uint256"}],"name":"e0","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v26","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v34","type":"uint256"},{"indexed":false,"name":"v35","type":"uint256"}],"name":"e2","type":"event"}];

export const contractCode = "0x60806040526040516116103803806116108339818101604052604081101561002657600080fd5b81019080805190602001909291908051906020019092919050505060008282604051602001808481526020018373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b815260140193505050506040516020818303038152906040528051906020012060001c600081905550505061151f806100f16000396000f3fe6080604052600436106100865760003560e01c8063908ab22a11610059578063908ab22a146105835780639f54e22e14610667578063aed03b1014610708578063b0041610146107bc578063cea70b7d1461084857610086565b806302b040e81461008b57806314728a4e146101cc578063865ca4e7146103a4578063873398b914610426575b600080fd5b34801561009757600080fd5b50610151600480360360208110156100ae57600080fd5b81019080803590602001906401000000008111156100cb57600080fd5b8201836020820111156100dd57600080fd5b803590602001918460018302840111640100000000831117156100ff57600080fd5b91908080601f016020809104026020016040519081016040528093929190818152602001838380828437600081840152601f19601f820116905080830192505050505050509192919290505050610989565b6040518080602001828103825283818151815260200191508051906020019080838360005b83811015610191578082015181840152602081019050610176565b50505050905090810190601f1680156101be5780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b3480156101d857600080fd5b50610329600480360360408110156101ef57600080fd5b810190808035906020019064010000000081111561020c57600080fd5b82018360208201111561021e57600080fd5b8035906020019184600183028401116401000000008311171561024057600080fd5b91908080601f016020809104026020016040519081016040528093929190818152602001838380828437600081840152601f19601f820116905080830192505050505050509192919290803590602001906401000000008111156102a357600080fd5b8201836020820111156102b557600080fd5b803590602001918460018302840111640100000000831117156102d757600080fd5b91908080601f016020809104026020016040519081016040528093929190818152602001838380828437600081840152601f19601f8201169050808301925050505050505091929192905050506109b7565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561036957808201518184015260208101905061034e565b50505050905090810190601f1680156103965780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b610424600480360360a08110156103ba57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803590602001909291908035906020019092919080359060200190929190505050610a88565b005b34801561043257600080fd5b506105086004803603606081101561044957600080fd5b810190808035906020019064010000000081111561046657600080fd5b82018360208201111561047857600080fd5b8035906020019184600183028401116401000000008311171561049a57600080fd5b91908080601f016020809104026020016040519081016040528093929190818152602001838380828437600081840152601f19601f820116905080830192505050505050509192919290803561ffff169060200190929190803561ffff169060200190929190505050610c79565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561054857808201518184015260208101905061052d565b50505050905090810190601f1680156105755780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b34801561058f57600080fd5b50610649600480360360208110156105a657600080fd5b81019080803590602001906401000000008111156105c357600080fd5b8201836020820111156105d557600080fd5b803590602001918460018302840111640100000000831117156105f757600080fd5b91908080601f016020809104026020016040519081016040528093929190818152602001838380828437600081840152601f19601f820116905080830192505050505050509192919290505050610d60565b604051808261ffff1661ffff16815260200191505060405180910390f35b610706600480360361010081101561067e57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803590602001909291908035906020019092919080359060200190929190803590602001909291908035906020019092919080359060200190929190505050610dd4565b005b34801561071457600080fd5b506107416004803603602081101561072b57600080fd5b8101908080359060200190929190505050611277565b6040518080602001828103825283818151815260200191508051906020019080838360005b83811015610781578082015181840152602081019050610766565b50505050905090810190601f1680156107ae5780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b610846600480360360c08110156107d257600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803590602001909291908035906020019092919080359060200190929190803590602001909291905050506112a0565b005b34801561085457600080fd5b5061090e6004803603602081101561086b57600080fd5b810190808035906020019064010000000081111561088857600080fd5b82018360208201111561089a57600080fd5b803590602001918460018302840111640100000000831117156108bc57600080fd5b91908080601f016020809104026020016040519081016040528093929190818152602001838380828437600081840152601f19601f8201169050808301925050505050505091929192905050506114cd565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561094e578082015181840152602081019050610933565b50505050905090810190601f16801561097b5780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b6060600061099683610d60565b600201905060008184510390506109ae848383610c79565b92505050919050565b6060825183836040516020018084815260200183805190602001908083835b602083106109f957805182526020820191506020810190506020830392506109d6565b6001836020036101000a03801982511681845116808217855250505050505090500182805190602001908083835b60208310610a4a5780518252602082019150602081019050602083039250610a27565b6001836020036101000a0380198251168184511680821785525050505050509050019350505050604051602081830303815290604052905092915050565b60008585604051602001808481526020018373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b815260140193505050506040516020818303038152906040528051906020012060001c60005414610b2e57600080fd5b8473ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614610b6657600080fd5b8183013414610b7457600080fd5b7fe97939a5c3b22414f653aea9f18c5859841d0af50ba6ede8dd8f1937b8da453883838360405180848152602001838152602001828152602001935050505060405180910390a160018585858585604051602001808781526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b815260140184815260200183815260200182815260200196505050505050506040516020818303038152906040528051906020012060001c6000819055505050505050565b60608161ffff168361ffff160184511015610c9357600080fd5b60608261ffff166040519080825280601f01601f191660200182016040528015610ccc5781602001600182028038833980820191505090505b5090506000838501905060008090505b8161ffff168161ffff161015610d5357868161ffff1681518110610cfc57fe5b602001015160f81c60f81b838261ffff1681518110610d1757fe5b60200101907effffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff1916908160001a9053508080600101915050610cdc565b5081925050509392505050565b6000600282511015610d7157600080fd5b600082600181518110610d8057fe5b602001015160f81c60f81b60f81c60ff1661010084600081518110610da157fe5b602001015160f81c60f81b60f81c60ff16020190508061ffff1660020183511015610dcb57600080fd5b80915050919050565b6002888888888888604051602001808881526020018773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018581526020018481526020018381526020018281526020019750505050505050506040516020818303038152906040528051906020012060001c60005414610e9a57600080fd5b8773ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614610ed257600080fd5b60003414610edf57600080fd5b610f27826040516020018082815260200191505060405160208183030381529060405282604051602001808281526020019150506040516020818303038152906040526109b7565b6040516020018082805190602001908083835b60208310610f5d5780518252602082019150602081019050602083039250610f3a565b6001836020036101000a0380198251168184511680821785525050505050509050019150506040516020818303038152906040528051906020012060001c8414610fa657600080fd5b60008114610fc75760018114610fbf5760028114610fc2565b60015b610fca565b60015b610fd357600080fd5b6000808214610ff55760018214610fed5760028214610ff0565b60015b610ff8565b60015b9050600080851461101c57600185146110145760028514611017565b60015b61101f565b60015b905060008261102f576000611031565b815b611056578261104e5781611046576001611049565b60005b611051565b60025b611067565b60038660040385018161106557fe5b065b90506000811461108a57600181146110825760028114611085565b60015b61108d565b60015b61109657600080fd5b600281146110a55760016110a8565b60005b6110d557600084146110cd57600184146110c557600284146110c8565b60015b6110d0565b60015b6110d8565b60015b6110e157600080fd5b600081146110f05760016110f3565b60005b611120576000861461111857600186146111105760028614611113565b60015b61111b565b60015b611123565b60015b61112c57600080fd5b6000600282149050600080831490508c73ffffffffffffffffffffffffffffffffffffffff166108fc8361116e5782611167578b8d01611169565b8b5b611175565b8b8d600202015b9081150290604051600060405180830381858888f193505050501580156111a0573d6000803e3d6000fd5b508b73ffffffffffffffffffffffffffffffffffffffff166108fc836111d557826111cb578c6111d0565b8c6002025b6111d8565b60005b9081150290604051600060405180830381858888f19350505050158015611203573d6000803e3d6000fd5b507f3a074aaea2d51955da8b38c6383720deafa2c498a47a8b8312010f91a80d49d38787604051808381526020018281526020019250505060405180910390a1600080819055507302b463784bc1a49f1647b47a19452ac420dfc65a73ffffffffffffffffffffffffffffffffffffffff16ff5b606081604051602001808281526020019150506040516020818303038152906040529050919050565b60018686868686604051602001808781526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b815260140184815260200183815260200182815260200196505050505050506040516020818303038152906040528051906020012060001c6000541461135e57600080fd5b8473ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff161461139657600080fd5b8334146113a257600080fd5b600081146113c357600181146113bb57600281146113be565b60015b6113c6565b60015b6113cf57600080fd5b7f3680e78b6fdf571695c81f108d81181ea63f50c100e6375e765b14bd7ac0adbb816040518082815260200191505060405180910390a16002868686868686604051602001808881526020018773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018581526020018481526020018381526020018281526020019750505050505050506040516020818303038152906040528051906020012060001c600081905550505050505050565b60606114e38260026114de85610d60565b610c79565b905091905056fea265627a7a7230582024ab1b6c8cfa6894233211c2c0ef8fee25c7230e28d533dbd8da250799be712664736f6c63430005090032";
