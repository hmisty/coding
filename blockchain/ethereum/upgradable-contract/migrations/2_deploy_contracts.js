var ModuleAFactory = artifacts.require("ModuleAFactory");
var ModuleAImpl = artifacts.require("ModuleAImpl");

module.exports = function(deployer) {
	deployer.deploy(ModuleAFactory);
	deployer.deploy(ModuleAImpl);
}
