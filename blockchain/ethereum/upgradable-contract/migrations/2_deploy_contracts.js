const AppFactory = artifacts.require("AppFactory");
const AppImpl = artifacts.require("AppImpl");
const Membership = artifacts.require("Membership");
const KeyValueStorage = artifacts.require("KeyValueStorage");
const Managed = artifacts.require("Managed");

module.exports = function(deployer) {
	// for app test
	deployer.deploy(Membership);
	deployer.link(Membership, AppImpl);
	deployer.deploy(AppImpl);
	deployer.deploy(AppFactory);

	// for framework test cases
	deployer.deploy(KeyValueStorage);
	deployer.deploy(Managed);
}
