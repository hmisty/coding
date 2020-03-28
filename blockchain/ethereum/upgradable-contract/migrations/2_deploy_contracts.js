const AppFactory = artifacts.require("AppFactory");
const App = artifacts.require("App");
const AppImpl = artifacts.require("AppImpl");
const Membership = artifacts.require("Membership");
const KeyValueStorage = artifacts.require("KeyValueStorage");
const Managed = artifacts.require("Managed");
const Owned = artifacts.require("Owned");
const Module = artifacts.require("Module");

module.exports = function(deployer) {
	// for app test
	deployer.deploy(Membership);
	deployer.link(Membership, AppImpl);
	deployer.deploy(AppImpl);
	deployer.deploy(AppFactory);
	deployer.deploy(App); //only for testing deployment

	// for framework test cases
	deployer.deploy(KeyValueStorage);
	deployer.deploy(Managed);
	deployer.deploy(Owned);
	deployer.deploy(Module);
}
