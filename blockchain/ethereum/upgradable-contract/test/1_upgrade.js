/**
 * test cases for verifying the upgradability of the App, including:
 * 1. deployment
 * 2. soft upgrade
 * 3. hard upgrade
 */

const AppFactory = artifacts.require("AppFactory");
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
	it("should create then create gain succeeds", async() => {
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

});