/**
 * test cases for contracts/upgradable/Managed.sol, including:
 * 1. onlyManager modifier 
 * 2. isRunning modifier 
 * 3. change manager
 * 4. pause/unpause
 *
 * How to Use:
 * 1. add to migrations/2_deploy_contracts.js:
 * deployer.deploy(Managed);
 *
 * 2. $ truffle develop
 *
 * 3. truffle(develop)> migrate
 * 4. truffle(develop)> test "test/0_managed.js"
 *
 */
const truffleAssert = require('truffle-assertions');

const AppFactory = artifacts.require("AppFactory");
const Managed = artifacts.require("Managed");

contract("Managed", accounts => {

	/**
	 * test deployment
	 */
	it("should the managed deployed()", async () => {
		var managed_deployed = await Managed.deployed();
		var managed_address = managed_deployed.address;
		assert.notEqual(managed_address, 0);
	});

	/**
	 * test onlyManager, isRunning modifier, pause/unpause and changeManager
	 */
	it("should onlyManager to changeManager while isRunning", async () => {
		var managed_deployed = await Managed.deployed();
		var managed_address = managed_deployed.address;

		// the object
		var man = new web3.eth.Contract(Managed.abi, managed_address);

		// check the current manager
		var manager = await man.methods.manager().call();
		assert.equal(manager, accounts[0]);

		// check the running status first
		var running = await man.methods.running().call();
		assert.equal(running, true);

		// calling pause() with the right manager
		await man.methods.setRunning(false).send({from: accounts[0]});
		running = await man.methods.running().call();
		assert.equal(running, false);

		// calling unpause with the wrong manager
		// this tx should fail
		truffleAssert.reverts(man.methods.setRunning(true).send({from: accounts[1]}));
		running = await man.methods.running().call();
		assert.notEqual(running, true); // shouldn't effect

		// changeManager while is not running
		// this tx should fail
		truffleAssert.reverts(man.methods.changeManager(accounts[1]).send({from: accounts[0]}));

		manager = await man.methods.manager().call();
		assert.notEqual(manager, accounts[1]); // shouldn't effect
		
		// calling unpause with the right manager
		await man.methods.setRunning(true).send({from: accounts[0]});
		running = await man.methods.running().call();
		assert.equal(running, true);

		// changeManager while is running
		await man.methods.changeManager(accounts[1]).send({from: accounts[0]});
		var manager = await man.methods.manager().call();
		assert.equal(manager, accounts[1]);

	});

});
