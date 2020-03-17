pragma solidity >=0.4.22 <0.5.0;

import "./Owned.sol";
import "./KeyValueStorage.sol";

/**
 * Allow contracts to be upgradable.
 */
contract upgradable is owned {
    
    /**
     * returns the external storage address
     */
    function getStorage() view public returns (address) {
        return address(_storage);
    }

    /**
     * Step 2. Setup the storage for the new module.
     * 
     * Only manager can initialize.
     */
    function setupStorage(address _legacyStorage) isRunning onlyManager public {
        // initialize storage
        if (_legacyStorage != address(0x0)) {
            // use legacy storage if having one
            _storage = KeyValueStorage(_legacyStorage);
            // still no access, need to wait for _legacyStorage.upgradeTo() done.
        } else if (address(_storage) == address(0x0)) {
            // otherwise create a new storage
            _storage = new KeyValueStorage(); // manager of the storage is Module
        }
    }
    
    /**
     * Step 3. Transfer the fund from the old module to the new module.
     * 
     * Only manager can upgrade.
     */
    function upgradeTo(address _newModule) isRunning onlyManager public {
        // transfer total fund to new module
        //_newModule.transfer(getBalance());
        bool success = upgradable(_newModule).receiveFund.value(getBalance())();
        require(success, "fund transfer failed.");
        
        // transfer storage access
        _storage.changeManager(_newModule);
        
        // deprecate the storage of the old module for avoiding misuse.
        _storage = KeyValueStorage(address(0x0));
        
        // stop running
        halt();
    }
    
    /**
     * for checking the balance of the module.
     */
    function getBalance() view public returns (uint256) {
        return address(this).balance;
    }
    
    /**
     * for receiving upgrading fund transfer.
     */
    event FundReceived(address _sender, uint256 _amount);
    
    function receiveFund() external payable returns (bool) {
        emit FundReceived(msg.sender, msg.value);
        return true;
    }
}
