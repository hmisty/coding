/**
 * test cases for verifying the key-value storage, including:
 * 1. get/set address
 * 2. get/set uint256
 * 3. get/set bool
 * 4. get/set string
 * 5. get/set bytes32
 * 6. get/set int256
 *
 * How to Use:
 * 1. add to migrations/2_deploy_contracts.js:
 * deployer.deploy(KeyValueStorage);
 *
 * 2. $ truffle develop
 *
 * 3. truffle(develop)> migrate
 * 4. truffle(develop)> test "test/1_storage.js"
 *
 */

const AppFactory = artifacts.require("AppFactory");
const KeyValueStorage = artifacts.require("KeyValueStorage");

contract("KeyValueStorage", accounts => {

	/**
	 * create a storage for testing
	 */
	it("should the storage deployed()", async () => {
		var storage_deployed = await KeyValueStorage.deployed();
		var storage_address = storage_deployed.address;
		assert.notEqual(storage_address, 0);
	});

	/**
	 * test get/set address
	 */
	it("should get/set address", async () => {
		var storage_deployed = await KeyValueStorage.deployed();
		var storage_address = storage_deployed.address;

		var storage = new web3.eth.Contract(KeyValueStorage.abi, storage_address);

		// test get address
		var key = web3.utils.fromAscii("hello");
		var val = await storage.methods.getAddress(key).call();
		assert.equal(val, 0);

		// test set address
		var expect = accounts[0];
		await storage.methods.setAddress(key, expect).send({from: accounts[0]}); // not call()!
		var val = await storage.methods.getAddress(key).call();
		assert.equal(val, expect);
	});

	/**
	 * test get/set uint
	 */
	it("should get/set uint", async () => {
		var storage_deployed = await KeyValueStorage.deployed();
		var storage_address = storage_deployed.address;

		var storage = new web3.eth.Contract(KeyValueStorage.abi, storage_address);

		// test get uint
		var key = web3.utils.toHex("hello");
		var val = await storage.methods.getUint(key).call();
		assert.equal(val, 0);

		// test set uint
		var expect = 10;
		await storage.methods.setUint(key, expect).send({from: accounts[0]}); // not call()!
		var val = await storage.methods.getUint(key).call();
		assert.equal(val, expect);
	});

	/**
	 * test get/set bool
	 */
	it("should get/set bool", async () => {
		var storage_deployed = await KeyValueStorage.deployed();
		var storage_address = storage_deployed.address;

		var storage = new web3.eth.Contract(KeyValueStorage.abi, storage_address);

		// test get uint
		var key = web3.utils.toHex("the key");
		var val = await storage.methods.getUint(key).call();
		assert.equal(val, 0);

		// test set uint
		var expect = 1; // true will fail
		await storage.methods.setUint(key, expect).send({from: accounts[0]}); // not call()!
		var val = await storage.methods.getUint(key).call();
		assert.equal(val, expect);
	});

	/**
	 * test get/set string
	 */
	it("should get/set string", async () => {
		var storage_deployed = await KeyValueStorage.deployed();
		var storage_address = storage_deployed.address;

		var storage = new web3.eth.Contract(KeyValueStorage.abi, storage_address);

		// test get uint
		var key = "0x12345678".valueOf();
		var val = await storage.methods.getString(key).call();
		assert.equal(val, "");

		// test set uint
		var expect = "hello world";
		await storage.methods.setString(key, expect).send({from: accounts[0]}); // not call()!
		var val = await storage.methods.getString(key).call();
		assert.equal(val, expect);
	});

	/**
	 * test get/set bytes32
	 */
	it("should get/set bytes32", async () => {
		var storage_deployed = await KeyValueStorage.deployed();
		var storage_address = storage_deployed.address;

		var storage = new web3.eth.Contract(KeyValueStorage.abi, storage_address);

		// test get uint
		var key = "0x12345678".valueOf();
		var val = await storage.methods.getBytes(key).call();
		assert.equal(val, 0x0);

		// test set uint
		var expect = web3.utils.padLeft("0x123456789".valueOf(), 64);
		await storage.methods.setBytes(key, expect).send({from: accounts[0]}); // not call()!
		var val = await storage.methods.getBytes(key).call();
		assert.equal(val, expect);
	});

	/**
	 * test get/set int
	 */
	it("should get/set int", async () => {
		var storage_deployed = await KeyValueStorage.deployed();
		var storage_address = storage_deployed.address;

		var storage = new web3.eth.Contract(KeyValueStorage.abi, storage_address);

		// test get uint
		var key = "0x12345678".valueOf();
		var val = await storage.methods.getInt(key).call();
		assert.equal(val, 0x0);

		// test set uint
		var expect = 1024;
		await storage.methods.setInt(key, expect).send({from: accounts[0]}); // not call()!
		var val = await storage.methods.getInt(key).call();
		assert.equal(val, expect);
	});

});
