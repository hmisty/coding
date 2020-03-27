pragma solidity >=0.4.22 <0.5.0;

import "./Managed.sol";
import "./Module.sol";

/**
 * Maintains the registries of contracts for verification use.
 */
contract ModuleFactory is managed {
    ///////////////////////////////////////
    // error codes used in the framework //
    ///////////////////////////////////////
    /**
       code format: x.y.z
       x: 9 = this upgradable framework
       y: 0 = contract managed
          1 = contract owned
          2 = contract Module
          3 = contract ModuleImpl
          5 = contract KeyValueStorage
          9 = contract ModuleFactory
       z: 0 = first msg
          1 = second msg
          2 = ...

    // impl must not be 0x0
    string constant public FACTORY_REQUIRE_IMPL = "9.9.0";
    // module must not be 0x0
    string constant public FACTORY_REQUIRE_MODULE = "9.9.1";
    // legacy module must not be 0x0
    string constant public FACTORY_REQUIRE_LEGACY_MODULE = "9.9.2";
    // legacy storage must not be 0x0
    string constant public FACTORY_REQUIRE_LEGACY_STORAGE = "9.9.3";
    // legacy owner must not be 0x0
    string constant public FACTORY_REQUIRE_LEGACY_OWNER = "9.9.4";
    // legacy impl must not be 0x0
    string constant public FACTORY_REQUIRE_LEGACY_IMPL = "9.9.5";
    // only owner can do it
    string constant public FACTORY_REQUIRE_ONLY_OWNER = "9.9.6";
    */


    ///////////////////////////////////////
    // the current implementation
    address _implementation = address(0x0);
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

    // Event
    event ModuleCreated(address module, address stor, address owner);

    /**
     * Anyone can create a new module, just like
     * to register a new company at the government.
     */
    function create() public returns (address) {
        return createFor(msg.sender);
    }

    function createFor(address owner) public returns (address) {
        //require(_implementation != address(0x0), FACTORY_REQUIRE_IMPL);
        require(_implementation != address(0x0), "no impl");

        // FIXME 0.4.24 compatible, use address payable for 0.5.0
        address _newModuleAddr = newModule(); // its msg.sender will be the factory
        Module _newModule = Module(_newModuleAddr);

        _newModule.setupStorage(address(0x0));
        _newModule.setupOwner(owner);

        // automatically associate the current implementation for the module just created.
        _newModule.changeImplementation(_implementation);

        // emit EventCreated
        emit ModuleCreated(_newModuleAddr, _newModule.getStorage(), _newModule.getOwner());

        return address(_newModule);
    }

    /**
     * Only manager can create a new module from an old one.
     */
    // FIXME 0.4.24 compatible, use address payable for 0.5.0
    function createFrom(address _legacyModule) onlyManager public returns (address) {
        //require(_legacyModule != address(0x0), FACTORY_REQUIRE_LEGACY_MODULE);
        require(_legacyModule != address(0x0), "no legacy module");

        // retrieve important data from the legacy module.
        Module _oldModule = Module(_legacyModule);

        address owner = _oldModule.getOwner();
        address stor = _oldModule.getStorage();
        address impl = _oldModule.getImplementation();
        //require(stor != address(0x0), FACTORY_REQUIRE_LEGACY_STORAGE);
        //require(owner != address(0x0), FACTORY_REQUIRE_LEGACY_OWNER);
        //require(impl != address(0x0), FACTORY_REQUIRE_LEGACY_IMPL);
        require(stor != address(0x0), "no legacy storage");
        require(owner != address(0x0), "no legacy owner");
        require(impl != address(0x0), "no legacy impl");

        // create the new module.
        // FIXME 0.4.24 compatible, use address payable for 0.5.0
        address _newModuleAddr = newModule(); // its msg.sender will be the factory
        Module _newModule = Module(_newModuleAddr);

        _newModule.setupStorage(stor);

        /* No need to call _newModule.setupOwner to transfer owner
         * or _newModule.changeImplementation to transfer impl,
         * (and we are incapable to do so because _newModule has not
         * become the manager of the _storage yet,)
         * because both owner and implementation are now
         * already stored in the _storage.
         */

        return address(_newModule);
    }

    /********** managing the created module *************/
    // These functions in the module are not needed to expose:
    // changeManager, setupStorage

    // FIXME 0.4.24 compatible, use address payable for 0.5.0
    // start
    function unpause(address _module) onlyManager public {
        Module(_module).setRunning(true);
    }

    // stop
    function pause(address _module) onlyManager public {
        Module(_module).setRunning(false);
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
     * returns the current implementation address.
     */
    function getCurrentImplementation() view public returns (address) {
        return _implementation;
    }

    /**
     * Only manager can set the current implementation address.
     */
    function setCurrentImplementation(address _newImpl) onlyManager public {
        // no need to check _newImpl != address(0x0)
        // nor _newImpol != getImplementation(),
        // for compacting the code size
        
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
        // no need to check _newImpl != address(0x0)
        // nor _newImpol != getImplementation(),
        // for compacting the code size
        _availableImplementations[_newImpl] = true;
    }

    function unregisterImplementation(address _newImpl) onlyManager public {
        // no need to check _newImpl != address(0x0)
        // nor _availableImplementations[_newImpl] == true,
        // for compacting the code size
        _availableImplementations[_newImpl] = false;
    }

    /**
     * Only manager can change implementation of the module.
     */
    // FIXME 0.4.24 compatible, use address payable for 0.5.0
    function changeImplementation(address _module, address _newImpl) onlyManager public {
        //require(_module != address(0x0), FACTORY_REQUIRE_MODULE);
        require(_module != address(0x0), "no module");
        // no need to check _newImpl != address(0x0)
        // nor _availableImplementations[_newImpl] == true,
        // for compacting the code size
        Module(_module).changeImplementation(_newImpl);
    }

    /**
     * Only owner can update to the current implementation (the newest version).
     */
    // FIXME 0.4.24 compatible, use address payable for 0.5.0
    function updateImplementation(address _module) public {
        //require(_module != address(0x0), FACTORY_REQUIRE_MODULE);
        //require(msg.sender == Module(_module).getOwner(), FACTORY_REQUIRE_ONLY_OWNER);
        require(_module != address(0x0), "no module");
        require(msg.sender == Module(_module).getOwner(), "only owner");
        Module(_module).changeImplementation(_implementation);
    }
}
