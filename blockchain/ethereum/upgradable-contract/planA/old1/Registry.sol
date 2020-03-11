pragma solidity >=0.4.22 <0.6.0;

import "Manageable.sol";

/**
 * Maintains the registries of contracts for verification use.
 */
contract Registry is manageable {
    
    // the module contract instances and their registered addresses
    mapping(address => bool) public moduleRegistry;
    
    // the storage contract instances and their registered addresses
    mapping(address => bool) public storageRegistry;
    
    /**
     * Manager can register
     */
    function registerModule(address _newModule) onlyManager public {
        moduleRegistry[_newModule] = true;
    }
    
    function registerStorage(address _newStorage) onlyManager public {
        storageRegistry[_newStorage] = true;
    }
    
    /**
     * Anyone can verify
     */ 
    function isModuleRegistered(address _any) public view returns (bool) {
        return moduleRegistry[_any];
    }
    
    function isStorageRegistered(address _any) public view returns (bool) {
        return storageRegistry[_any];
    }
}
