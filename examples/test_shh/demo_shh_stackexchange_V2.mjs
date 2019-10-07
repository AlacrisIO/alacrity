import Web3            from 'web3';


var meth=3;
if (meth === 1) {
    console.log('Case meth=1');
    const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';
    var web3 = new Web3(new Web3.providers.HttpProvider(uri));
}

if (meth === 2) {
    console.log('Case meth=2');
    const eventProvider = new Web3.providers.WebsocketProvider('ws://localhost:8546');
    eventProvider.disconnect();
//    console.log('eventProvider=', eventProvider);
//    eventProvider.connection.close();
//    const eventProvider = new Web3.providers.HttpProvider('http://localhost:8545')
//    var web3 = new Web3(eventProvider);
//    web3.setProvider(eventProvider);
}

if (meth === 3) {
    console.log('Case meth=3');
    var web3_shh = new Web3('ws://localhost:8546')
    const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';
    var web3 = new Web3(new Web3.providers.HttpProvider(uri));
}



var identities = [];

console.log('Before Promises');
Promise.all([
    web3_shh.shh.newSymKey().then((id) => {
        console.log('id 1=', id);
        identities.push(id);
    }),
    web3_shh.shh.newKeyPair().then((id) => {
        console.log('id 2=', id);
        identities.push(id);
    })
]).then(() => {
    console.log('Before shh.suscribe');
    web3_shh.shh.subscribe('messages', {
        symKeyID: identities[0],
        topics: ['0xffaadd11']
    }).on('data', x => {
        console.log('Received message 1: x.payload=', x.payload);
    });
    web3_shh.shh.subscribe('messages', {
        symKeyID: identities[0],
        topics: ['0xffaadd22']
    }).on('data', x => {
        console.log('Received message 2: x.payload=', x.payload);
    });
    console.log('Exiting the code');
}).then(() => {
    console.log('Before shh.post');
    for (var n=0; n<100; n++)
    {
        const eVal = 25 + n;
        const payload = '0xabcd' + eVal.toString(16);
        const LTopic = [ '0xffaadd11', '0xffaadd22' ];
        const pos = Math.floor(Math.random() * 2);
        const topic = LTopic[pos];
        console.log('payload=', payload, ' topic=', topic);
        web3_shh.shh.post({
            symKeyID: identities[0], // encrypts using the sym key ID
            sig: identities[1], // signs the message using the keyPair ID
            topic,
            payload,
            ttl: 10,
            powTime: 3,
            powTarget: 0.5
        }).then(h => console.log(`Message with hash ${h} was successfuly sent`))
            .catch(err => console.log("Error: ", err));
    }
})
/*
    .catch(err => {
    console.log('Encountering error err=', err);
});*/
console.log('Final line of demo_shh file');
