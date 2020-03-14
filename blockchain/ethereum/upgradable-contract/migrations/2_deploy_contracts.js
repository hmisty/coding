var AppFactory = artifacts.require("AppFactory");
var AppImpl = artifacts.require("AppImpl");
var AppImpl2 = artifacts.require("AppImpl2");

module.exports = function(deployer) {
	deployer.deploy(AppFactory);
	deployer.deploy(AppImpl);
	deployer.deploy(AppImpl2);
}
