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

    ///////////////////////////////////////
    // the special key of owner in the _storage
    // using different hash algorithm to avoid conflict with other keys
    bytes32 internal constant __OWNER__ = sha256("__owner__");

    // the external key-value storage for this module.
    KeyValueStorage internal _storage;

    /**
     * returns the external storage address
     */
    function getStorage() view public returns (address) {
        return address(_storage);
    }

    /**
     * set the storage, just for test cases.
     * in normal scenario, only ModuleFactory, i.e. the manager of Module
     * can call this. however, ModuleFactory does not expose this function
     * to the external world.
     */
    function setStorage(KeyValueStorage _newStorage) onlyManager public {
        // no need to check _newStorage or _newStorage manager. this func should be only used for testing.
        _storage = _newStorage;
    }

    // owner
    event OwnerChanged(address _from, address _to);

    modifier onlyOwner {
        // no need to check storage because getOwner will do it.
        require(msg.sender == getOwner(), "only owner");
        _;
    }

    function getOwner() view public returns (address) {
        // no need to check _storage, 0x0.getAddress will fail anyway.
        return _storage.getAddress(__OWNER__);
    }

    /**
     * requires owner of manager to change the owner
     */
    function changeOwner(address _newOwner) isRunning public {
        // allows only owner or manager to do this
        require(msg.sender == manager || msg.sender == getOwner(), "only manager or owner");
        
        // no need to check _storage, 0x0.setAddress will fail anyway.
        address _oldOwner = getOwner();
        _storage.setAddress(__OWNER__, _newOwner);
        emit OwnerChanged(_oldOwner, _newOwner);
    }

}
