/**
 * test cases for contracts/upgradable/Module.sol, including:
 * 1. 
 *
 * How to Use:
 * 1. add to migrations/2_deploy_contracts.js:
 * deployer.deploy(Module);
 *
 * 2. $ truffle develop
 *
 * 3. truffle(develop)> migrate
 * 4. truffle(develop)> test "test/0_module.js"
 *
 */
const truffleAssert = require('truffle-assertions');

const AppFactory = artifacts.require("AppFactory");
const KeyValueStorage = artifacts.require("KeyValueStorage");
const AppImpl = artifacts.require("AppImpl");
const Module = artifacts.require("Module");

contract("Module", accounts => {

	var contract;

	/**
	 * test deployment
	 */
	it("should the Module deployed()", async () => {
		var module_deployed = await Module.deployed();
		var module_address = module_deployed.address;
		assert.notEqual(module_address, 0);

		contract = new web3.eth.Contract(Module.abi, module_address);
	});

	/**
	 * test setup a new storage
	 */
	it("should setup a new storage", async () => {
		// check storage is not initialized
		var stor = await contract.methods.getStorage().call();
		assert.equal(stor, 0x0);

		var module_deployed = await Module.deployed();
		var zero_address = web3.utils.padLeft(0, 40);
		await module_deployed.methods["setupStorage(address)"].sendTransaction(zero_address);
		stor = await contract.methods.getStorage().call();
		assert.notEqual(stor, 0x0);

		// check manager of the storage
		var storage = new web3.eth.Contract(KeyValueStorage.abi, stor);
		var manager = await storage.methods.manager().call();
		assert.notEqual(manager, accounts[0]);
		assert.equal(manager, contract._address);

	});

	/**
	 * test setup a legacy storage
	 */
	it("should setup a legacy storage", async () => {
		var storage_deployed = await KeyValueStorage.deployed();
		var storage_address = storage_deployed.address;

		// save the old storage address
		var old_address = await contract.methods.getStorage().call();

		// setup a legacy storage
		/*
		var module_deployed = await Module.deployed();
		await module_deployed.methods["setupStorage(address)"].sendTransaction(storage_address);
		stor = await contract.methods.getStorage().call();
		assert.equal(stor, storage_address);
		assert.notEqual(stor, old_address);
		*/
		storage_deployed.changeManager(contract._address);
		await contract.methods.setStorage(storage_deployed.address).send({from: accounts[0]});
		stor = await contract.methods.getStorage().call();
		assert.notEqual(stor, old_address);
		assert.equal(stor, storage_address);

		// check manager of the storage
		var storage = new web3.eth.Contract(KeyValueStorage.abi, stor);
		var manager = await storage.methods.manager().call();
		assert.notEqual(manager, accounts[0]);
		assert.equal(manager, contract._address);

	});

	/**
	 * test upgrade to a new module
	 * as well as getBalance and receiveFund
	 */
	it("should upgrade to a new module", async () => {
		var storage_deployed = await KeyValueStorage.deployed();
		var storage_address = storage_deployed.address;

		// save some fund into contract
		await contract.methods.receiveFund().send({from: accounts[0], value: 100000});
		var balance = await contract.methods.getBalance().call();
		assert.equal(balance, 100000);

		// deploy on demand
		var module2 = await Module.new();
		// setup legacy storage
		await module2.setupStorage(storage_address);
		// upgrade
		await contract.methods.upgradeTo(module2.address).send({from: accounts[0]});

		// check old module status
		var old_storage = await contract.methods.getStorage().call();
		var old_paused = await contract.methods.paused().call();
		var old_balance = await contract.methods.getBalance().call();
		assert.equal(old_storage, 0x0);
		assert.equal(old_paused, true);
		assert.equal(old_balance, 0);
		
		// check old module status
		var new_storage = await module2.getStorage();
		var new_paused = await module2.paused();
		var new_balance = await module2.getBalance();
		assert.equal(new_storage, storage_address);
		assert.equal(new_paused, false);
		assert.equal(new_balance, 100000);

		// check manager of the storage
		var storage = new web3.eth.Contract(KeyValueStorage.abi, stor);
		var manager = await storage.methods.manager().call();
		assert.notEqual(manager, contract._address);
		assert.equal(manager, module2.address);

		// change testing contract to module2
		contract = new web3.eth.Contract(Module.abi, module2.address);
	});

	/**
	 * test change implementation and route function calls
	 */
	it("should change implementation and route function calls", async () => {
		var impl = await AppImpl.deployed();
		var impl_address = impl.address;

		// check initial implementation
		var using_impl = await contract.methods.implementation().call();
		assert.equal(using_impl, 0x0);

		// change
		await contract.methods.changeImplementation(impl_address).send({from: accounts[0]});
		using_impl = await contract.methods.implementation().call();
		assert.equal(using_impl, impl_address);

		// route
		var m = new web3.eth.Contract(AppImpl.abi, contract._address);
		var ver = await m.methods.getVersionTag().call();
		assert.equal(ver, "0.0.1");

	});

});
