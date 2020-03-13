pragma solidity >=0.4.22 <0.6.0;

import "./Managed.sol";
import "./Module.sol";

/**
 * Maintains the registries of contracts for verification use.
 */
contract ModuleFactory is managed {

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
     * Abstract function TO BE IMPLEMENTED.
     * returns a new instance of Module.
     */ 
    // FIXME 0.4.24 compatible, use address payable for 0.5.0
    function newModule() public returns (address);

    /**
     * Anyone can create a new module, just like
     * to register a new company at the government.
     */
    function create() public returns (address) {
        return createFor(msg.sender);
    }

    function createFor(address owner) public returns (address) {
        require(_implementation != address(0x0), "no implementation ready to use.");

        // FIXME 0.4.24 compatible, use address payable for 0.5.0
        address _newModuleAddr = newModule(); // its msg.sender will be the factory
        Module _newModule = Module(_newModuleAddr);

        _newModule.setupStorage(address(0x0));
        _newModule.setupOwner(owner);

        // automatically associate the current implementation for the module just created.
        _newModule.changeImplementation(_implementation);

        return address(_newModule);
    }

    /**
     * Only manager can create a new module from an old one.
     */
    // FIXME 0.4.24 compatible, use address payable for 0.5.0
    function createFrom(address _legacyModule) onlyManager public returns (address) {
        require(_legacyModule != address(0x0), "legacy module not exists.");

        // retrieve important data from the legacy module.
        Module _oldModule = Module(_legacyModule);

        address owner = _oldModule.getOwner();
        address stor = _oldModule.getStorage();
        address impl = _oldModule.implementation();
        require(owner != address(0x0), "legacy module has no owner.");
        require(stor != address(0x0), "legacy module has no storage.");
        require(impl != address(0x0), "legacy module has no implementation.");

        // create the new module.
        // FIXME 0.4.24 compatible, use address payable for 0.5.0
        address _newModuleAddr = newModule(); // its msg.sender will be the factory
        Module _newModule = Module(_newModuleAddr);

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

    // FIXME 0.4.24 compatible, use address payable for 0.5.0
    function start(address _module) onlyManager public {
        Module(_module).start();
    }

    function halt(address _module) onlyManager public {
        Module(_module).halt();
    }

    function setupOwner(address _module, address _newOwner) public onlyManager {
        Module(_module).setupOwner(_newOwner);
    }

    /**
     * Only manager can upgrade legacyModule to new one.
     * NOTICE: this function cannot be combined with createFrom
     * createFrom is called by the old factory.
     * upgrade will be called by the new factory.
     */
    // FIXME 0.4.24 compatible, use address payable for 0.5.0
    function upgrade(address _legacyModule, address _newModule) onlyManager public {
        Module(_legacyModule).upgradeTo(_newModule);
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
    // FIXME 0.4.24 compatible, use address payable for 0.5.0
    function changeImplementation(address _module, address _newImpl) onlyManager public {
        require(_module != address(0x0), "module not exists.");
        require(_newImpl != address(0x0), "implementation not exists.");
        require(_availableImplementations[_newImpl] == true, "implementation not exists.");
        Module(_module).changeImplementation(_newImpl);
    }

    /**
     * Only owner can update to the current implementation (the newest version).
     */
    // FIXME 0.4.24 compatible, use address payable for 0.5.0
    function updateImplementation(address _module) public {
        require(_module != address(0x0), "module not exists.");
        require(Module(_module).getOwner() == msg.sender, "only owner can do it.");
        Module(_module).changeImplementation(_implementation);
    }
}
