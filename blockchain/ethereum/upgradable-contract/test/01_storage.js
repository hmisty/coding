/**
 * test cases for verifying the key-value storage. features vs coverage:
 * 1. get/set address: YES covered.
 * 2. get/set uint256: YES covered.
 * 3. get/set bool: YES covered.
 * 4. get/set string: YES covered.
 * 5. get/set bytes32: YES covered.
 * 6. get/set int256: YES covered.
 * 7. fail to set anything when not isRunning: YES covered.
 *
 * How to Use:
 * 1. add to migrations/2_deploy_contracts.js:
 * deployer.deploy(KeyValueStorage);
 * 2. $ truffle develop
 * 3. truffle(develop)> test "test/01_storage.js"
 *
 */
const truffleAssert = require('truffle-assertions');

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

		// test get bool
		var key = web3.utils.toHex("the key");
		var val = await storage.methods.getBool(key).call();
		assert.equal(val, false);

		// test set bool 
		var expect = true;
		await storage.methods.setBool(key, expect).send({from: accounts[0]}); // not call()!
		var val = await storage.methods.getBool(key).call();
		assert.equal(val, expect);
	});

	/**
	 * test get/set string
	 */
	it("should get/set string", async () => {
		var storage_deployed = await KeyValueStorage.deployed();
		var storage_address = storage_deployed.address;

		var storage = new web3.eth.Contract(KeyValueStorage.abi, storage_address);

		// test get string
		var key = "0x12345678".valueOf();
		var val = await storage.methods.getString(key).call();
		assert.equal(val, "");

		// test set string
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

		// test get bytes32
		var key = "0x12345678".valueOf();
		var val = await storage.methods.getBytes(key).call();
		assert.equal(val, 0x0);

		// test set bytes32
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

		// test get int
		var key = "0x12345678".valueOf();
		var val = await storage.methods.getInt(key).call();
		assert.equal(val, 0x0);

		// test set int
		var expect = 1024;
		await storage.methods.setInt(key, expect).send({from: accounts[0]}); // not call()!
		var val = await storage.methods.getInt(key).call();
		assert.equal(val, expect);
	});

	/**
	 * test fail to set anything when not isRunning
	 */
	it("should fail to set anything when not isRunning", async () => {
		var storage_deployed = await KeyValueStorage.deployed();
		var storage_address = storage_deployed.address;

		var storage = new web3.eth.Contract(KeyValueStorage.abi, storage_address);

		// stop the contract
		await storage.methods.setRunning(false).send({from: accounts[0]});

		// test set uint
		var key = web3.utils.fromAscii("hello");
		var expect = 1024;
		truffleAssert.reverts(storage.methods.setInt(key, expect).send({from: accounts[0]}));
		// test set bytes32
		var expect = web3.utils.padLeft("0x123456789".valueOf(), 64);
		truffleAssert.reverts(storage.methods.setBytes(key, expect).send({from: accounts[0]}));
		// test set string
		var expect = "hello world";
		truffleAssert.reverts(storage.methods.setString(key, expect).send({from: accounts[0]}));
		// test set bool 
		var expect = true;
		truffleAssert.reverts(storage.methods.setBool(key, expect).send({from: accounts[0]}));
		// test set uint
		var expect = 10;
		truffleAssert.reverts(storage.methods.setUint(key, expect).send({from: accounts[0]}));
		// test set address
		var expect = accounts[0];
		truffleAssert.reverts(storage.methods.setAddress(key, expect).send({from: accounts[0]}));

		// restart the deployed contract
        // otherwise the following test cases (e.g. test/02_owned.js) might be affected...
		await storage.methods.setRunning(true).send({from: accounts[0]});
	});

});
