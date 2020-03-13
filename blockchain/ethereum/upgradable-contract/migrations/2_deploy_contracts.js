var AppFactory = artifacts.require("AppFactory");
var AppImpl = artifacts.require("AppImpl");

module.exports = function(deployer) {
	deployer.deploy(AppFactory);
	deployer.deploy(AppImpl);
}
