var AppFactory = artifacts.require("AppFactory");
var AppImpl = artifacts.require("AppImpl");
var Membership = artifacts.require("Membership");
var KeyValueStorage = artifacts.require("KeyValueStorage");

module.exports = function(deployer) {
	// for app test
	deployer.deploy(Membership);
	deployer.link(Membership, AppImpl);
	deployer.deploy(AppImpl);
	deployer.deploy(AppFactory);

	// for standalone test cases
	deployer.deploy(KeyValueStorage);
}
