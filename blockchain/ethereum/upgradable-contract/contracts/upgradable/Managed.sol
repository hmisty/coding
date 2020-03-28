pragma solidity >=0.4.22 <0.5.0;

/**
 * Allow contracts to be managable to upgrade contracts as well as doing other admin actions, etc.
 *
 * Overall speaking, manager should have the capability to:
 * 1, halt the contracts for a while, to be ready for any unexpected emergency
 * 2, restart the halted contracts to return to normal
 * 3, change the addresses of all children contracts (for upgradation)
 * 4, change the manager address itself for transferring the responsibility to the next one
 */
contract managed {
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

    // only manager can do it
    string constant public MANAGED_REQUIRE_ONLY_MANAGER = "9.0.0";
    */

    /////////////////////////////////////
    //      manager and functions      //
    /////////////////////////////////////
    address public manager; // who have the permission to maintain the system

    event ManagerChanged(address indexed _from, address indexed _to);

    /**
     * setup the manager to the deployer while deployed
     * could be possible to be overriden
     */
    constructor() public {
        manager = msg.sender; // this should be the factory address
    }

    /**
     * The modifier for decorating functions.
     */
    modifier onlyManager {
        //require(msg.sender == manager, MANAGED_REQUIRE_ONLY_MANAGER);
        require(msg.sender == manager, "only manager");
        _;
    }

    /**
     * Stoppable.
     * only manager can halt/start the contract
     */
    bool public running = true;

    modifier isRunning {
        require(running);
        _;
    }

    // "stop" conflicts with the assembly. use "pause".
    // Warning: Variable is shadowed in inline assembly by an instruction of the same name
    // use only one function instead of two (pause/unpause, start/stop) to reduce the code size.
    function setRunning(bool _running) onlyManager public {
        running = _running;
    }

    /**
     * only manager can change manager
     */
    function changeManager(address _newManager) isRunning public onlyManager {
        // code optimization to reduce code size
        emit ManagerChanged(manager, _newManager);
        manager = _newManager;
    }
}
