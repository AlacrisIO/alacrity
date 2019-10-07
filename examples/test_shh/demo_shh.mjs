import Web3            from 'web3';


const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';
var web3 = new Web3(new Web3.providers.HttpProvider(uri));

var shh = web3.shh;
//var Shh = require('web3-shh');
//var shh = new Shh(Shh.givenProvider || 'ws://some.local-or-remote.node:8546');


var identities = [];
var subscription = null;
console.log('Before Promise.all');
Promise.all([
    web3.shh.newSymKey().then((id) => {identities.push(id);}),
    web3.shh.newKeyPair().then((id) => {identities.push(id);})

]).then(() => {
    console.log('Before suscription');
    // will receive also its own message send, below
    subscription = web3.shh.subscribe('messages', {
        symKeyID: identities[0],
        topics: ['0xffaadd11']
    }).on('data', console.log);

}).then(() => {
    console.log('Before shh.post');
    web3.shh.post({
        symKeyID: identities[0], // encrypts using the sym key ID
        sig: identities[1], // signs the message using the keyPair ID
        ttl: 10,
        topic: '0xffaadd11',
        payload: '0xffffffdddddd1122',
        powTime: 3,
        powTarget: 0.5
    }).then(h => console.log(`Message with hash ${h} was successfuly sent`))
        .catch(err => console.log("Error: ", err));
});
