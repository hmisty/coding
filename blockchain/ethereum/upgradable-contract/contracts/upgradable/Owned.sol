pragma solidity >=0.4.22 <0.5.0;

import "./Managed.sol";
import "./KeyValueStorage.sol";

/**
 * owned = own an eternal shared storage.
 *
 * The owner is stored in the key value storage with a special key:
 * KEY_OWNER = 0x00000000000000000000000000000000000000000000000000000000000000ff
 *
 */
contract owned is managed {
    
    // the encoded bytes of the special key "owner"
    bytes32 constant KEY_OWNER = 0x00000000000000000000000000000000000000000000000000000000000000ff;
    
    // the external key-value storage for this module.
    KeyValueStorage _storage;

    /**
     * returns the external storage address
     */
    function getStorage() view public returns (address) {
        return address(_storage);
    }

    /**
     * initialize the storage
     * donot setStorage, because we must let Owned mananges the storage
     */
    function initStorage() onlyManager public {
        require(address(_storage) == address(0x0), "storage already initialized.");
        _storage = new KeyValueStorage();
    }

    // owner
    event OwnerChanged(address _from, address _to);

    modifier onlyOwner {
        require(address(_storage) != address(0x0), "storage not initialized.");
        require(getOwner() == msg.sender, "only owner can do this.");
        _;
    }
    
    function getOwner() view public returns (address) {
        require(address(_storage) != address(0x0), "storage not initialized.");
        return _storage.getAddress(KEY_OWNER);
    }
    
    function changeOwner(address _newOwner) isRunning public onlyOwner {
        require(address(_storage) != address(0x0), "storage not initialized.");

        address _oldOwner = getOwner();
        _storage.setAddress(KEY_OWNER, _newOwner);
        emit OwnerChanged(_oldOwner, _newOwner);
    }
    
    // Manager can change owner too
    function setupOwner(address _newOwner) isRunning public onlyManager {
        require(address(_storage) != address(0x0), "storage not initialized.");

        address _oldOwner = getOwner();
        _storage.setAddress(KEY_OWNER, _newOwner);
        emit OwnerChanged(_oldOwner, _newOwner);
    }

}
