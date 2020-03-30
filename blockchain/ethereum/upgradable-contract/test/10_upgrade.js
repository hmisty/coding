/**
 * test cases for verifying the upgradability of the App, including:
 * 1. deployment
 * 2. soft upgrade
 * 3. hard upgrade
 *
 * features vs coverage:
 * 1. create & createFor, isRunning, impl != 0x0: YES covered.
 * 2. createFrom, onlyManager: YES covered.
 * 3. pause/unpause, onlyManager: YES covered.
 * 4. changeOwner, onlyManager: YES covered.
 * 5. upgrade, onlyManager: YES covered.
 * 6. get/set current implementation, set onlyManager: YES covered.
 * 7. changeImplementation, onlyManager: YES covered.
 * 8. updateImplementation, isRunning, only owner: YES covered.
 */
const truffleAssert = require('truffle-assertions');

const AppFactory = artifacts.require("AppFactory");
const App = artifacts.require("App");
const AppImpl = artifacts.require("AppImpl");
const AppImpl2 = artifacts.require("AppImpl2");

contract("AppFactory", accounts => {

	// shared variable by all cases.
	var _app_address;

	/**
	 * test the Factory to create an new App
	 */
	it("should create an App that has the AppImpl", async () => {
		var factory = await AppFactory.deployed();
		var impl = await AppImpl.deployed();
		//console.log(factory);

		// prepare to create an App
		var m = factory.methods["create()"];

		// no impl, cannot create
		truffleAssert.reverts(m.call()); // should fail

		// use impl
		// test only manager
		truffleAssert.reverts(factory.setCurrentImplementation(impl.address, {from: accounts[1]})); // should fail
		var tx = await factory.setCurrentImplementation(impl.address);
		//console.log(tx);

		// stop the contract
		await factory.setRunning(false);
		truffleAssert.reverts(m.call()); // should fail
		// restart the contract
		await factory.setRunning(true);

		// can create() & createFor() now
		var app_address = await m.call(); 
		//console.log(app_address);

		tx = await m.sendTransaction();
		//console.log(tx);
		
		// save the app_address for later testcase use.
		this._app_address = app_address;

		// verify the App's impl
		var app = new web3.eth.Contract(AppImpl.abi, app_address);
		var ver = await app.methods.getVersionTag().call();
		assert.equal(ver, "0.0.1");
	});

	/**
	 * test only manager can change implementation
	 */
	it("should only manager can change implementation", async () => {
		var factory = await AppFactory.deployed();
		var impl = await AppImpl.deployed();

		// change impl
		var zero_address = web3.utils.padLeft(0, 40);
		truffleAssert.reverts(factory.changeImplementation(this._app_address, zero_address, {from: accounts[1]})); // should fail
		await factory.changeImplementation(this._app_address, zero_address); // should succeed
		// verify, should not effect
		var app = new web3.eth.Contract(App.abi, this._app_address);
		var impl_addr = await app.methods.getImplementation().call();
		assert.notEqual(impl_addr, 0x0);

		// try again with non-zero address
		var fake_address = web3.utils.padLeft(1, 40);
		await factory.changeImplementation(this._app_address, fake_address);
		// verify, should effect
		var impl_addr2 = await app.methods.getImplementation().call();
		assert.equal(impl_addr2, 0x1);

		// change back
		await factory.changeImplementation(this._app_address, impl.address);
		// verify
		var impl_addr3 = await app.methods.getImplementation().call();
		assert.equal(impl_addr3, impl.address);

	});

	/**
	 * test only manager can pause/unpause an app
	 */
	it("should only manager can pause/unpause an app", async () => {
		var factory = await AppFactory.deployed();

		// pause
		truffleAssert.reverts(factory.pause(this._app_address, {from: accounts[1]})); // should fail
		await factory.pause(this._app_address); // should succeed
		// verify
		var app = new web3.eth.Contract(App.abi, this._app_address);
		var running = await app.methods.running().call();
		assert.equal(running, false);

		// unpause
		truffleAssert.reverts(factory.unpause(this._app_address, {from: accounts[1]})); // should fail
		await factory.unpause(this._app_address); // should succeed
		// verify
		var running = await app.methods.running().call();
		assert.equal(running, true);

	});

	/**
	 * test only manager can change owner
	 */
	it("should only manager can change owner", async () => {
		var factory = await AppFactory.deployed();

		// change owner to accounts[1]
		truffleAssert.reverts(factory.changeOwner(this._app_address, accounts[1], {from: accounts[1]})); // should fail
		await factory.changeOwner(this._app_address, accounts[1]); // should succeed
		// verify
		var app = new web3.eth.Contract(App.abi, this._app_address);
		var owner = await app.methods.getOwner().call();
		assert.equal(owner, accounts[1]);

		// change owner back to accounts[0]
		truffleAssert.reverts(factory.changeOwner(this._app_address, accounts[0], {from: accounts[1]})); // should fail
		await factory.changeOwner(this._app_address, accounts[0]); // should succeed
		// verify
		var owner = await app.methods.getOwner().call();
		assert.equal(owner, accounts[0]);

	});

	/**
	 * test the soft upgrade from AppImpl to AppImpl2
	 */
	it("should soft upgrade from AppImpl to AppImpl2", async () => {
		var factory = await AppFactory.deployed();
		var impl = await AppImpl.deployed();
		//var impl2 = await AppImpl2.deployed();
		// deploy on demand, so we won't need it in migration
		var impl2 = await AppImpl2.new(); 
		//console.log(factory);

		// verify the current implementation == impl.address
		var impl_addr = await factory.getCurrentImplementation();
		//console.log(impl_addr);
		assert.equal(impl_addr, impl.address);

		// change it to impl2
		var tx = await factory.setCurrentImplementation(impl2.address);
		//console.log(tx);

		/* the contexts in artifacts is remained
		 * so we won't need to create the app again!
		 */
		/*
		// create an App
		var m = factory.methods["create()"];
		var app_address = await m.call();
		//console.log(app_address);

		tx = await m.sendTransaction();
		//console.log(tx);
		*/

		// retrieve the app_address from artifacts contexts
		var app_address = this._app_address;

		// stop
		await factory.setRunning(false);
		// update the app's implementation to the latest version :)
		truffleAssert.reverts(factory.updateImplementation(app_address)); // should fail
		// restart
		await factory.setRunning(true);
		// not owner, can call, but no change
		await factory.updateImplementation(app_address, {from: accounts[1]});

		// verify the App's impl
		var app = new web3.eth.Contract(AppImpl.abi, app_address);
		var ver = await app.methods.getVersionTag().call();
		assert.equal(ver, "0.0.1");

		// owner to update impl
		await factory.updateImplementation(app_address, {from: accounts[0]});

		// changed
		var ver = await app.methods.getVersionTag().call();
		assert.equal(ver, "0.0.2");
	});

	/**
	 * test a specific bug reported on 3/18:
	 * set an impl => create => success
	 * set another impl => create => nothing?
	 */
	it("should create then create again succeeds", async() => {
		var factory = await AppFactory.deployed();

		// new impl
		var impl1 = await AppImpl.new(); 
		var tx = await factory.setCurrentImplementation(impl1.address);

		// create an App
		var m = factory.methods["create()"];
		var app_address = await m.call();
		//console.log(app_address);

		tx = await m.sendTransaction();
		//console.log(tx);

		// new another impl
		var impl2 = await AppImpl2.new();
		tx = await factory.setCurrentImplementation(impl2.address);

		// create another App
		var m = factory.methods["create()"];
		app_address = await m.call();
		//console.log(app_address);

		tx = await m.sendTransaction();
		//console.log(tx);

		// verify the App's impl
		var app = new web3.eth.Contract(AppImpl2.abi, app_address);
		var ver = await app.methods.getVersionTag().call();
		assert.equal(ver, "0.0.2");
	});

	/**
	 * test hard upgrade from App v1 to App v2
	 */
	it("should hard upgrade App v1 to App v2", async () => {
		var factory = await AppFactory.deployed();

		var app1_address = this._app_address;
		
		// save impl for later use
		var app1 = new web3.eth.Contract(App.abi, app1_address);
		var impl_address = await app1.methods.getImplementation().call();

		// save some fund into app1
		await app1.methods.receiveFund().send({from: accounts[0], value: 100000});
		var balance1 = await app1.methods.getBalance().call();
		assert.equal(balance1, 100000);

		var storage1 = await app1.methods.getStorage().call();
		assert.notEqual(storage1, 0x0);

		// create App v2 from App v1
		var m = factory.methods["createFrom(address)"];
		var app2_address = await m.call(app1_address);
		// test only manager
		truffleAssert.reverts(m.sendTransaction(app1_address, {from: accounts[1]})); // should fail
		await m.sendTransaction(app1_address);
		
		// hard upgrade
		truffleAssert.reverts(factory.upgrade(app1_address, app2_address, {from: accounts[1]})); // should fail
		await factory.upgrade(app1_address, app2_address);

		// check app1 status
		var old_storage = await app1.methods.getStorage().call();
		var old_running = await app1.methods.running().call();
		var old_balance = await app1.methods.getBalance().call();
		assert.equal(old_storage, 0x0);
		assert.equal(old_running, false);
		assert.equal(old_balance, 0);
		
		// check app2 status
		var app2 = new web3.eth.Contract(App.abi, app2_address);
		var new_storage = await app2.methods.getStorage().call();
		var new_running = await app2.methods.running().call();
		var new_balance = await app2.methods.getBalance().call();
		assert.equal(new_storage, storage1);
		assert.equal(new_running, true);
		assert.equal(new_balance, balance1);

		var new_owner = await app2.methods.getOwner().call();
		var new_impl = await app2.methods.getImplementation().call();
		assert.equal(new_owner, accounts[0]);
		assert.equal(new_impl, impl_address);

		// check app2 versionTag (inherits impl2 from app1)
		app2 = new web3.eth.Contract(AppImpl2.abi, app2_address);
		var ver = await app2.methods.getVersionTag().call();
		assert.equal(ver, "0.0.2");
	});

});
