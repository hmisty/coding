pragma solidity >=0.4.22 <0.6.0;

import "./Managed.sol";
import "./ModuleA.sol";

/**
 * Maintains the registries of contracts for verification use.
 */
contract ModuleAFactory is managed {

    // the current implementation
    address public _implementation = address(0x0);
    // all available implementations
    mapping(address => bool) _availableImplementations;

    /**
     * NOTICE: Let only the system manager deploy!
     * 
     */
    constructor() public {
        manager = msg.sender; // this should be the system manager
    }
    
    /**
     * Anyone can create a new module, just like
     * to register a new company at the government.
     */
    function create() public returns (address) {
        return createFor(msg.sender);
    }
    
    function createFor(address owner) public returns (address) {
        require(_implementation != address(0x0), "no implementation ready to use.");
        
        ModuleA _newModule = new ModuleA(); // its msg.sender will be the factory
        _newModule.setupStorage(address(0x0));
        _newModule.setupOwner(owner);
        
        // automatically associate the current implementation for the module just created.
        _newModule.changeImplementation(_implementation);
        
        return address(_newModule);
    }

    /**
     * Only manager can create a new module from an old one.
     */
    function createFrom(address payable _legacyModule) onlyManager public returns (address) {
        require(_legacyModule != address(0x0), "legacy module not exists.");
        
        // retrieve important data from the legacy module.
        ModuleA _oldModule = ModuleA(_legacyModule);
        
        address owner = _oldModule.getOwner();
        address stor = _oldModule.getStorage();
        address impl = _oldModule.implementation();
        require(owner != address(0x0), "legacy module has no owner.");
        require(stor != address(0x0), "legacy module has no storage.");
        require(impl != address(0x0), "legacy module has no implementation.");
        
        // create the new module.
        ModuleA _newModule = new ModuleA(); // its msg.sender will be the factory
        _newModule.setupStorage(stor);
        //_newModule.setupOwner(owner); // already in the storage, no need to transfer again
        
        // automatically inherit the legacy implementataion.
        // NOTICE: we won't register the legacy implementataion automatically,
        // for it's neither mandatory nor good.
        _newModule.changeImplementation(impl);

        return address(_newModule);
    }
    
    /********** managing the created module *************/
    // These functions in the module are not needed to expose: 
    // changeManager, setupStorage
    
    function start(address payable _module) onlyManager public {
        ModuleA(_module).start();
    }
    
    function halt(address payable _module) onlyManager public {
        ModuleA(_module).halt();
    }
    
    function setupOwner(address payable _module, address _newOwner) public onlyManager {
        ModuleA(_module).setupOwner(_newOwner);
    }
    
    /**
     * Only manager can upgrade legacyModule to new one.
     * NOTICE: this function cannot be combined with createFrom
     * createFrom is called by the old factory.
     * upgrade will be called by the new factory.
     */
    function upgrade(address payable _legacyModule, address _newModule) onlyManager public {
        ModuleA(_legacyModule).upgradeTo(_newModule);
    }
    
    /**
     * Only manager can set the current implementation address.
     */
    function setCurrentImplementation(address _newImpl) onlyManager public {
        require(_newImpl != address(0x0), "implementation not exists.");
        require(_newImpl != _implementation, "already the current implementation.");
        // register it first if not yet.
        if (_availableImplementations[_newImpl] == false) {
            registerImplementation(_newImpl);
        }
        // then set it to the current implementation.
        _implementation = _newImpl;
    }
 
    /**
     * Only manager can register/unregister a new implementation address.
     */
    function registerImplementation(address _newImpl) onlyManager public {
        require(_newImpl != address(0x0), "implementation not exists.");
        require(_availableImplementations[_newImpl] == false, "already registered.");
        _availableImplementations[_newImpl] = true;
    }
    
    function unregisterImplementation(address _newImpl) onlyManager public {
        require(_newImpl != address(0x0), "implementation not exists.");
        require(_availableImplementations[_newImpl] == true, "implementation not exists.");
        _availableImplementations[_newImpl] = false;
    }
    
    /**
     * Only manager can change implementation of the module.
     */
    function changeImplementation(address payable _module, address _newImpl) onlyManager public {
        require(_module != address(0x0), "module not exists.");
        require(_newImpl != address(0x0), "implementation not exists.");
        require(_availableImplementations[_newImpl] == true, "implementation not exists.");
        ModuleA(_module).changeImplementation(_newImpl);
    }
    
    /**
     * Only owner can update to the current implementation (the newest version).
     */
    function updateImplementation(address payable _module) public {
        require(_module != address(0x0), "module not exists.");
        require(ModuleA(_module).getOwner() == msg.sender, "only owner can do it.");
        ModuleA(_module).changeImplementation(_implementation);
    }
}
