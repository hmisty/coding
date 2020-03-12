pragma solidity >=0.4.22 <0.6.0;

/**
 * Allow contracts to be managable to upgrade contracts as well as doing other admin actions, etc.
 */
contract manageable {
    address internal INITIAL_MANAGER = 0xe9366a50d16a064d76996d4713c0fB1F66848Dc6; // initial
    
    address public manager = 0x0; // who have the permission to maintain the system
    
    event ManagerChanged(address indexed _from, address indexed _to);
    
    constructor() public {
        // DO NOT set manager to msg.sender!
        // Module contracts will inherit this contract, thus
        // the msg.sender will be the owner (deployer) of that speicific Module instance.
        manager = INITIAL_MANAGER;
    }
    
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
    
    // 4
    function changeManager(address _newManager) public onlyManager {
        address oldManager = manager;
        manager = _newManager;
        emit ManagerChanged(oldManager, _newManager);
    }
}
