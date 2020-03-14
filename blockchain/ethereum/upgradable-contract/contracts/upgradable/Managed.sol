pragma solidity >=0.4.22 <0.5.0;

/**
 * Allow contracts to be managable to upgrade contracts as well as doing other admin actions, etc.
 */
contract managed {
    address public manager = address(0x0); // who have the permission to maintain the system
    
    event ManagerChanged(address indexed _from, address indexed _to);

    /**
     * The modifier for decorating functions.
     */ 
    modifier onlyManager {
        require(msg.sender == manager, "only manager can do it");
        _;
    }
    
    /** TODO: Overall speaking, manager should have the capability to:
     * 1, halt the contracts for a while, to be ready for any unexpected emergency
     * 2, restart the halted contracts to return to normal
     * 3, change the addresses of all children contracts (for upgradation)
     * 4, change the manager address itself for transferring the responsibility to the next one
     */
    
    // 1 & 2
    /**
     * Stoppable.
     * only manager can halt/start the contract
     */
    bool public halted = false;

    modifier isRunning {
        require(!halted);
        _;
    }
    
    // "stop" conflicts with the assembly. use "halt".
    function halt() onlyManager public {
        halted = true;
    }

    function start() onlyManager public {
        halted = false;
    }
    
    // 4
    function changeManager(address _newManager) public onlyManager {
        address oldManager = manager;
        manager = _newManager;
        emit ManagerChanged(oldManager, _newManager);
    }
}
