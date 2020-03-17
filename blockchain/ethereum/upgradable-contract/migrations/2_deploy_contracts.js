var AppFactory = artifacts.require("AppFactory");
var AppImpl = artifacts.require("AppImpl");
var KeyValueStorage = artifacts.require("KeyValueStorage");

module.exports = function(deployer) {
	deployer.deploy(AppFactory);
	deployer.deploy(AppImpl);
	deployer.deploy(KeyValueStorage);
}
