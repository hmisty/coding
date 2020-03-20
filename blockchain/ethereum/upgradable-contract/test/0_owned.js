/**
 * test cases for contracts/upgradable/Owned.sol, including:
 * 1. onlyOwner modifier 
 * 2. get the owned storage
 * 3. get owner
 * 4. onlyOwner change owner 
 * 5. onlyManager setupOwner
 *
 * How to Use:
 * 1. add to migrations/2_deploy_contracts.js:
 * deployer.deploy(Owned);
 *
 * 2. $ truffle develop
 *
 * 3. truffle(develop)> migrate
 * 4. truffle(develop)> test "test/0_owned.js"
 *
 */
const truffleAssert = require('truffle-assertions');

const AppFactory = artifacts.require("AppFactory");
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
	it("should get/init the storage", async () => {
		// check storage is not initialized
		var stor = await contract.methods.getStorage().call();
		assert.equal(stor, 0x0);

		// init
		// this will fail
		//await contract.methods.initStorage().send({from: accounts[0]});
		var owned_deployed = await Owned.deployed();
		// this will succeed
		await owned_deployed.methods["initStorage()"].sendTransaction();
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

		// setup owner, onlyManager
		truffleAssert.reverts(contract.methods.setupOwner(accounts[1]).send({from: accounts[1]})); // should fail
		await contract.methods.setupOwner(accounts[1]).send({from: accounts[0]}); // should succeed
		owner = await contract.methods.getOwner().call();
		assert.equal(owner, accounts[1]);

		// change owner, onlyOwner
		truffleAssert.reverts(contract.methods.changeOwner(accounts[0]).send({from: accounts[0]})); // should fail
		await contract.methods.changeOwner(accounts[0]).send({from: accounts[1]}); // should succeed
		owner = await contract.methods.getOwner().call();
		assert.equal(owner, accounts[0]);

	});

});
