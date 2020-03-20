/**
 * test cases for verifying the upgradability of the App, including:
 * 1. deployment
 * 2. soft upgrade
 * 3. hard upgrade
 */

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

		// use impl
		var tx = await factory.setCurrentImplementation(impl.address);
		//console.log(tx);

		// create an App
		var m = factory.methods["create()"];
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
		// update the app's implementation to the latest version :)
		tx = await factory.updateImplementation(app_address);
		//console.log(tx);

		// verify the App's impl
		var app = new web3.eth.Contract(AppImpl.abi, app_address);
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

		// save some fund into app1
		var app1 = new web3.eth.Contract(App.abi, app1_address);
		await app1.methods.receiveFund().send({from: accounts[0], value: 100000});
		var balance1 = await app1.methods.getBalance().call();
		assert.equal(balance1, 100000);

		var storage1 = await app1.methods.getStorage().call();
		assert.notEqual(storage1, 0x0);

		// create App v2 from App v1
		var m = factory.methods["createFrom(address)"];
		var app2_address = await m.call(app1_address);
		await m.sendTransaction(app1_address);
		
		// hard upgrade
		await factory.upgrade(app1_address, app2_address);

		// check app1 status
		var old_storage = await app1.methods.getStorage().call();
		var old_paused = await app1.methods.paused().call();
		var old_balance = await app1.methods.getBalance().call();
		assert.equal(old_storage, 0x0);
		assert.equal(old_paused, true);
		assert.equal(old_balance, 0);
		
		// check app2 status
		var app2 = new web3.eth.Contract(App.abi, app2_address);
		var new_storage = await app2.methods.getStorage().call();
		var new_paused = await app2.methods.paused().call();
		var new_balance = await app2.methods.getBalance().call();
		assert.equal(new_storage, storage1);
		assert.equal(new_paused, false);
		assert.equal(new_balance, balance1);

		// check app2 versionTag (inherits impl2 from app1)
		app2 = new web3.eth.Contract(AppImpl2.abi, app2_address);
		var ver = await app2.methods.getVersionTag().call();
		assert.equal(ver, "0.0.2");
	});

});
