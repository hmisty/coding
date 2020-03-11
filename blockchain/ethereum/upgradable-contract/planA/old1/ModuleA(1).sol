pragma solidity >=0.4.22 <0.6.0;

import "SafeMath.sol";
import "Manageable.sol";
import "KeyValueStorage.sol";
import "Registry.sol";

/**
 * The ModuleA contract that is upgradable (i.e. relocated to a new contract).
 * 
 * Data is permanently stored in an external storage contract.
 * Fund will be transferred to the newly upgraded contract.
 */
contract ModuleA is manageable {
    using SafeMath for uint256;
    
    // the external key-value storage for this module.
    address public _storage = 0x0;
    
    // the registry for avoiding arbitrary module or storage contract.
    address public _registry = 0x0;

    /**
     * Pausable.
     * only manager can pause/resume the contract
     */
    bool public paused = false;

    modifier isRunning {
        require(!paused);
        _;
    }
    
    function pause() onlyManager public {
        paused = true;
    }

    function resume() onlyManager public {
        paused = false;
    }
    
    // owner
    address internal owner = 0x0;
    
    event OwnerChanged(address indexed _from, address indexed _to);

    modifier onlyOwner {
        require(owner == msg.sender);
        _;
    }
    
    function changeOwner(address _newOwner) public onlyOwner {
        address oldOwner = owner;
        owner = _newOwner;
        emit OwnerChanged(oldOwner, _newOwner);
    }
    
    /**
     * Manager can change owner too, to ease the manager 
     * to upgrade the contract for the owner.
     */
    function changeOwner2(address _newOwner) public onlyManager {
        address oldOwner = owner;
        owner = _newOwner;
        emit OwnerChanged(oldOwner, _newOwner);
    }

    /**
     * Owner to pay to deploy.
     */
    constructor() payable public {
        if (msg.sender == manager) {
            // manager is waived.
        } else {
            // TODO check the funding rule first.
            require(msg.value >= 10000, "not enough fund");
            
            owner = msg.sender;
        }
    }
    
    /**
     * Manager can change registry address.
     */
    function changeRegistry(address _newRegistry) isRunning onlyManager public {
        require(_newRegistry != 0x0, "new registry is null.");
        _registry = _newRegistry;
        Registry(_registry).registerModule(this);
    }

    /**
     * Manager can initialize the kvstorage
     * as a singleton.
     */ 
    function initializeStorage(address _legacyStorage) isRunning onlyManager public {
        // check _legacyStorage is registered.
        require(_registry != 0x0, "registry is not initialized.");
        require(_legacyStorage == 0x0 
            || Registry(_registry).isStorageRegistered(_legacyStorage),
            "unregistered legacy storage.");
        
        if (_legacyStorage != 0x0) {
            // use legacy storage if having one registered
            _storage = _legacyStorage;
        } else if (_storage == 0x0) {
            // otherwise create a new storage
            _storage = new KeyValueStorage();
            Registry(_registry).registerStorage(_storage);
        }
    }
    
    /**
     * Manager can transfer fund to upgraded module contract.
     */
    function transferTotalFundTo(address _newModule) isRunning onlyManager public {
        // check _newModule is registered.
        require(_registry != 0x0, "registry is not initialized.");
        require(_newModule == 0x0 
            || Registry(_registry).isModuleRegistered(_newModule),
            "unregistered module.");
        
        // transfer all fund to the new module instance
        _newModule.transfer(address(this).balance);
    }

    // Don't accept direct saving (ETH)
    function () external payable {
        revert();
    }

    ///////////////////////////////////////////////////
    //        free to implement anything below       //
    ///////////////////////////////////////////////////

    function setNumberOfMembers(uint256 num) isRunning public {
       KeyValueStorage(_storage).setUint("number-of-members", num);
    }
    
    function getNumberOfMembers() view public returns (uint256) {
        return KeyValueStorage(_storage).getUint("number-of-members");
    }
    
}
