import Web3            from 'web3';
const eventProvider = new Web3.providers.WebsocketProvider('ws://localhost:8546');
//    eventProvider.connection.close();
eventProvider.disconnect();
