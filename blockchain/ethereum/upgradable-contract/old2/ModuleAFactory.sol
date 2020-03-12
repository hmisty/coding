pragma solidity >=0.4.22 <0.6.0;

import "Managed.sol";
import "ModuleA_0_0_3.sol"; // change here for upgrades

/**
 * Maintains the registries of contracts for verification use.
 */
contract ModuleAFactory is managed {
    
    /**
     * Only manager can create a new module, just like
     * to register a new company at the government.
     */
    function create(address owner) onlyManager public returns (address) {
        ModuleA _newModule = new ModuleA();
        _newModule.setupStorage(0x0);
        _newModule.setupOwner(owner);
        return _newModule;
    }

    /**
     * Only manager can create a new module from an old one.
     */
    function createFrom(address _legacyModule) onlyManager public returns (address) {
        require(_legacyModule != 0x0, "legacy module not exists.");
        ModuleA _oldModule = ModuleA(_legacyModule);
        
        address owner = _oldModule.getOwner();
        address stor = _oldModule.getStorage();
        require(owner != 0x0, "legacy module has no owner");
        require(stor != 0x0, "legacy module has no storage");
        
        ModuleA _newModule = new ModuleA();
        _newModule.setupStorage(owner);
        _newModule.setupOwner(stor);
        return _newModule;
    }
}
