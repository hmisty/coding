/**
 * test cases for contracts/upgradable/Owned.sol. features vs coverage:
 * 1. get the owned _storage: YES covered.
 * 2. onlyManager init the _storage: YES covered.
 * 3. onlyOwner modifier: YES covered.
 * 4. get owner: YES covered.
 * 5. owner/manager can change owner: YES covered.
 * 6. fail to change owner when not isRunning: YES covered.
 *
 * How to Use:
 * 1. add to migrations/2_deploy_contracts.js:
 * deployer.deploy(Owned);
 * 2. $ truffle develop
 * 3. truffle(develop)> test "test/02_owned.js"
 *
 */
const truffleAssert = require('truffle-assertions');

const KeyValueStorage = artifacts.require("KeyValueStorage");
const Owned = artifacts.require("Owned");

contract("Owned", accounts => {

	var contract;

	/**
	 * test deployment
	 */
	it("should the owned deployed()", async () => {
		var owned_deployed = await Owned.deployed();
		var owned_address = owned_deployed.address;
		assert.notEqual(owned_address, 0);

		contract = new web3.eth.Contract(Owned.abi, owned_address);
	});

	/**
	 * test get/init storage
	 */
	it("should get/init the _storage", async () => {
		// check storage is not initialized
		var stor = await contract.methods.getStorage().call();
		assert.equal(stor, 0x0);

		// check manager of it
		var mgr = await contract.methods.manager().call();
		assert.equal(mgr, accounts[0]);

		/*
		// init
		// this will fail
		//await contract.methods.initStorage().send({from: accounts[0]});
		var owned_deployed = await Owned.deployed();
		// this will succeed
		await owned_deployed.methods["initStorage()"].sendTransaction();
		stor = await contract.methods.getStorage().call();
		assert.notEqual(stor, 0x0);
		*/
		var storage_deployed = await KeyValueStorage.deployed();
		truffleAssert.reverts(contract.methods.setStorage(storage_deployed.address).send({from: accounts[1]})); // should fail
		// must change manager first
		storage_deployed.changeManager(contract._address); 
		await contract.methods.setStorage(storage_deployed.address).send({from: accounts[0]});
		stor = await contract.methods.getStorage().call();
		assert.notEqual(stor, 0x0);

		// check manager of the storage
		var storage = new web3.eth.Contract(KeyValueStorage.abi, stor);
		var manager = await storage.methods.manager().call();
		assert.notEqual(manager, accounts[0]);
		assert.equal(manager, contract._address);

	});

	/**
	 * test get owner, setup owner, change owner and onlyOwner
	 */
	it("should get/setup/change owner", async () => {
		// check the current owner
		var owner = await contract.methods.getOwner().call();
		assert.equal(owner, 0x0);

		// check manager of the owned
		var manager = await contract.methods.manager().call();
		assert.equal(manager, accounts[0]);

		// manager to change owner
		// manager: accounts[0], owner: accounts[0]
		truffleAssert.reverts(contract.methods.changeOwner(accounts[1]).send({from: accounts[1]})); // should fail
		await contract.methods.changeOwner(accounts[1]).send({from: accounts[0]}); // should succeed
		owner = await contract.methods.getOwner().call();
		assert.equal(owner, accounts[1]);

		// owner to change owner
		// manager: accounts[0], owner: accounts[1]
		truffleAssert.reverts(contract.methods.changeOwner(accounts[0]).send({from: accounts[2]})); // should fail
		await contract.methods.changeOwner(accounts[0]).send({from: accounts[1]}); // should succeed
		owner = await contract.methods.getOwner().call();
		assert.equal(owner, accounts[0]);

	});

	/**
	 * test change owner when not isRunning
	 */
	it("should fail to change owner when not isRunning", async () => {
		// change owner to accounts[1]
		await contract.methods.changeOwner(accounts[1]).send({from: accounts[0]});

		// stop the contract
		await contract.methods.setRunning(false).send({from: accounts[0]});

		// owner to change owner, should fail
		truffleAssert.reverts(contract.methods.changeOwner(accounts[0]).send({from: accounts[1]}));
		// manager to change owner, should fail
		truffleAssert.reverts(contract.methods.changeOwner(accounts[0]).send({from: accounts[0]}));

		// restart the contract
		await contract.methods.setRunning(true).send({from: accounts[0]});

		// change owner back to accounts[0]
		await contract.methods.changeOwner(accounts[0]).send({from: accounts[0]}); // should succeed
	});

});
