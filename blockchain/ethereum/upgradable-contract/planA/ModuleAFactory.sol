pragma solidity >=0.4.22 <0.6.0;

import "Managed.sol";
import "ModuleA.sol";

/**
 * Maintains the registries of contracts for verification use.
 */
contract ModuleAFactory is managed {

    /**
     * NOTICE: Let only the system manager deploy!
     * 
     */
    constructor() public {
        manager = msg.sender; // this should be the system manager
    }
    
    /**
     * Only manager can create a new module, just like
     * to register a new company at the government.
     */
    function create(address owner) public returns (address) {
        ModuleA _newModule = new ModuleA(); // its msg.sender will be the factory
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
        
        ModuleA _newModule = new ModuleA(); // its msg.sender will be the factory
        _newModule.setupStorage(stor);
        //_newModule.setupOwner(owner); // already in the storage, no need to transfer again
        return _newModule;
    }
    
    /********** managing the created module *************/
    // These functions in the module are not needed to expose: 
    // changeManager, setupStorage
    
    function start(address _module) onlyManager public {
        ModuleA(_module).start();
    }
    
    function stop(address _module) onlyManager public {
        ModuleA(_module).stop();
    }
    
    function setupOwner(address _module, address _newOwner) public onlyManager {
        ModuleA(_module).setupOwner(_newOwner);
    }
    
    function upgrade(address _legacyModule, address _newModule) onlyManager public {
        ModuleA(_legacyModule).upgradeTo(_newModule);
    }
    
}
