pragma solidity >=0.4.22 <0.6.0;

/**
 * Allow contracts to be managable to upgrade contracts as well as doing other admin actions, etc.
 */
contract managed {
    address public manager = 0x0; // who have the permission to maintain the system
    
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
     * Pausable.
     * only manager can stop/start the contract
     */
    bool public stopped = false;

    modifier isRunning {
        require(!stopped);
        _;
    }
    
    function stop() onlyManager public {
        stopped = true;
    }

    function start() onlyManager public {
        stopped = false;
    }
    
    // 4
    function changeManager(address _newManager) public onlyManager {
        address oldManager = manager;
        manager = _newManager;
        emit ManagerChanged(oldManager, _newManager);
    }
}
