/**
 * test cases for contracts/upgradable/Module.sol. features vs coverage:
 * 1. setupStorage(0x0) - setup a new storage, only manager: YES covered.
 * 2. setupStorage(legacy) - setup with a legacy storage, only manager: YES covered.
 * 3. upgradeTo - upgrade to a new module, only manager: YES covered.
 * 4. getBalance: YES covered.
 * 5. receiveFund: YES covered.
 * 6. getImplementation: YES covered.
 * 7. changeImplementation: YES covered.
 * 8. route function call: YES covered.
 * 9. route call fail when impl = 0x0: YES covered.
 * 10. route call fail when not isRunning: YES covered.
 *
 * How to Use:
 * 1. add to migrations/2_deploy_contracts.js:
 * deployer.deploy(Module);
 * 2. $ truffle develop
 * 3. truffle(develop)> test "test/03_module.js"
 *
 */
const truffleAssert = require('truffle-assertions');

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
		// test only manager
		truffleAssert.reverts(module_deployed.methods["setupStorage(address)"].sendTransaction(zero_address, {from: accounts[1]})); // should fail
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
		// test only manager
		truffleAssert.reverts(contract.methods.setupStorage(storage_deployed.address).send({from: accounts[1]})); // should fail
		await contract.methods.setupStorage(storage_deployed.address).send({from: accounts[0]});
		stor = await contract.methods.getStorage().call();
		assert.notEqual(stor, old_address);
		assert.equal(stor, storage_address);

		// check manager of the storage
		// it is still the deployer address, i.e. accounts[0]
		var storage = new web3.eth.Contract(KeyValueStorage.abi, stor);
		var manager = await storage.methods.manager().call();
		assert.equal(manager, accounts[0]);

		// manually correct the manager of the storage to be the Module
		await storage_deployed.methods["changeManager(address)"].sendTransaction(contract._address);
		var mgr = await storage_deployed.methods["manager()"].call();
		assert.notEqual(mgr, accounts[0]);
		assert.equal(mgr, contract._address);

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
		var old_running = await contract.methods.running().call();
		var old_balance = await contract.methods.getBalance().call();
		assert.equal(old_storage, 0x0);
		assert.equal(old_running, false);
		assert.equal(old_balance, 0);
		
		// check new module status
		var new_storage = await module2.getStorage();
		var new_running = await module2.running();
		var new_balance = await module2.getBalance();
		assert.equal(new_storage, storage_address);
		assert.equal(new_running, true);
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
		var using_impl = await contract.methods.getImplementation().call();
		assert.equal(using_impl, 0x0);

		// test route call fail when impl = 0x0
		var m = new web3.eth.Contract(AppImpl.abi, contract._address);
		truffleAssert.reverts(m.methods.getVersionTag().call());

		// change impl
		// test only manager
		truffleAssert.reverts(contract.methods.changeImplementation(impl_address).send({from: accounts[1]})); // should fail
		await contract.methods.changeImplementation(impl_address).send({from: accounts[0]});
		using_impl = await contract.methods.getImplementation().call();
		assert.equal(using_impl, impl_address);

		// stop the contract
		await m.methods.setRunning(false).send({from: accounts[0]});
		// route call should fail when not isRunning
		truffleAssert.reverts(m.methods.getVersionTag().call());
		
		// restart the contract
		await m.methods.setRunning(true).send({from: accounts[0]});

		// route call should succeed
		var ver = await m.methods.getVersionTag().call();
		assert.equal(ver, "0.0.1");

	});

});
