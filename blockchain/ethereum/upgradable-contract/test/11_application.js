/**
 * test cases for verifying the App business logic, just for demonstration purpose:
 * 1. get version tag
 * 2. enable
 * 3. get/set number of members
 */

const AppFactory = artifacts.require("AppFactory");
const AppImpl = artifacts.require("AppImpl");

contract("App", accounts => {

	// shared variable by all cases.
	var _app_address;

	/**
	 * 1. test get version tag of the app (the newly created)
	 */
	it("should have the version tag 0.0.1", async () => {
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
		//console.log(app_address);

		// verify the App's impl version tag
		var app = new web3.eth.Contract(AppImpl.abi, app_address);
		//console.log(app);
		var ver = await app.methods.getVersionTag().call();
		//console.log(ver);
		assert.equal(ver, "0.0.1");
	});

	/**
	 * 2. test the enable() function of the App
	 */
	it("should enable an App", async () => {
		/*
		 * just simple reuse the existing App instance
		 *
		 */
		// retrieve the app_address from artifacts contexts
		var app_address = this._app_address;
		//console.log(app_address);

		// test enable()
		var app = new web3.eth.Contract(AppImpl.abi, app_address);
		var fenable = app.methods.enable();
		//console.log(fenable);
		tx = await fenable.send({from:accounts[0], value:100000});
		//console.log(tx);
		
		var balance = await app.methods.getBalance().call();
		assert.equal(balance, 100000);
	});

	/**
	 * 3. test the App functions to set/get number of memebers
	 */
	it("should set/get number of members", async () => {
		/*
		 * just simple reuse the existing App instance
		 *
		 */
		// retrieve the app_address from artifacts contexts
		var app_address = this._app_address;
		//console.log(app_address);

		// test set/get number of members
		var app = new web3.eth.Contract(AppImpl.abi, app_address);
		var fset = app.methods.setNumberOfMembers(100);
		//console.log(fset);
		tx = await fset.send({from:accounts[0]});
		//console.log(tx);
		
		var nMembers = await app.methods.getNumberOfMembers().call();
		assert.equal(nMembers, 100);
	});

	/**
	 * test the strange issue with isMember & isAdmin
	 */
	it("should check isMember and isAdmin", async () => {
		// retrieve the app_address from artifacts contexts
		var app_address = this._app_address;
		//console.log(app_address);

		// test set/get number of members
		var app = new web3.eth.Contract(AppImpl.abi, app_address);
		
		// check
		var is_member = await app.methods.isMember(accounts[0]).call();
		assert.equal(is_member, false);

		// check
		var is_admin = await app.methods.isAdmin(accounts[0]).call();
		assert.equal(is_admin, false);
	});
});
