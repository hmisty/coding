pragma solidity >=0.4.22 <0.6.0;

import "Managed.sol";
import "KeyValueStorage.sol";

/**
 * Allow contracts to be upgradable.
 */
contract upgradable is managed {
        
    // the external key-value storage for this module.
    KeyValueStorage public _storage;

    function getStorage() view public returns (address) {
        return _storage;
    }
    
    // owner
    event OwnerChanged(address indexed _from, address indexed _to);

    modifier onlyOwner {
        require(address(_storage) != 0x0, "storage not initialized.");
        require(getOwner() == msg.sender, "only owner can do this.");
        _;
    }
    
    function getOwner() view public returns (address) {
        require(address(_storage) != 0x0, "storage not initialized.");
        return _storage.getAddress("owner");
    }
    
    function changeOwner(address _newOwner) public onlyOwner {
        require(address(_storage) != 0x0, "storage not initialized.");

        address _oldOwner = getOwner();
        _storage.setAddress("owner", _newOwner);
        emit OwnerChanged(_oldOwner, _newOwner);
    }
    
    // Manager can change owner too, for upgrading
    function setupOwner(address _newOwner) public onlyManager {
        require(address(_storage) != 0x0, "storage not initialized.");

        address _oldOwner = getOwner();
        _storage.setAddress("owner", _newOwner);
        emit OwnerChanged(_oldOwner, _newOwner);
    }

    /**
     * Step 2. Setup the storage for the new module.
     * 
     * Only manager can initialize.
     */
    function setupStorage(address _legacyStorage) isRunning onlyManager public {
        // initialize storage
        if (_legacyStorage != 0x0) {
            // use legacy storage if having one
            _storage = KeyValueStorage(_legacyStorage);
        } else if (address(_storage) == 0x0) {
            // otherwise create a new storage
            _storage = new KeyValueStorage();
        }
    }
    
    /**
     * Step 3. Transfer the fund from the old module to the new module.
     * 
     * Only manager can upgrade.
     */
    function upgradeTo(address _newModule) isRunning onlyManager public {
        // transfer total fund to new module
        _newModule.transfer(address(this).balance);
        
        // deprecate the storage of the old module for avoiding misuse.
        _storage = KeyValueStorage(0x0);
        
        // stop running
        stop();
    }

    // Don't accept direct saving (ETH)
    function () external payable {
        revert();
    }
    
}
