pragma solidity >=0.4.22 <0.5.0;

import "./KeyValueStorage.sol";
import "./Managed.sol";

/**
 * owned = own an eternal shared storage.
 *
 * The owner is stored in the key value storage with a special key
 *
 */
contract owned is managed {
    
    // the encoded bytes of the special key
    // (randomly generated to avoid key conflicts)
    bytes32 constant KEY_OWNER = 0x5a56315fb445b1a5ac55db632cdcaa16c04666bd7c0ca5f6a2808b5709b7b12c;
    
    // the external key-value storage for this module.
    KeyValueStorage _storage;

    /**
     * returns the external storage address
     */
    function getStorage() view public returns (address) {
        return address(_storage);
    }

    /**
     * set the storage, just for test cases
     */
    function setStorage(KeyValueStorage _newStorage) onlyManager public {
        require(address(_newStorage) != address(0x0), "new storage is 0x0.");
        require(_newStorage.manager() == address(this), "new storage is not managed by me.");

        _storage = _newStorage;
    }

    // owner
    event OwnerChanged(address _from, address _to);

    modifier onlyOwner {
        // no need to check storage because getOwner will do it.
        require(getOwner() == msg.sender, "only owner can do this.");
        _;
    }
    
    function getOwner() view public returns (address) {
        require(address(_storage) != address(0x0), "storage not initialized.");
        return _storage.getAddress(KEY_OWNER);
    }
    
    function changeOwner(address _newOwner) isRunning public onlyOwner {
        // no need to check storage because onlyOwner has done it.
        // code optimization to reduce code size
        emit OwnerChanged(getOwner(), _newOwner);
        _storage.setAddress(KEY_OWNER, _newOwner);
    }
    
    // let Manager can change owner too
    function setupOwner(address _newOwner) isRunning public onlyManager {
        // no need to check storage because getOwner will do it.
        address _oldOwner = getOwner();
        _storage.setAddress(KEY_OWNER, _newOwner);
        emit OwnerChanged(_oldOwner, _newOwner);
    }
}
