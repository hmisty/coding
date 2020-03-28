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

    // only owner can do it
    string constant public OWNED_REQUIRE_ONLY_OWNER = "9.1.0";
    // _storage must not be 0x0
    string constant public OWNED_REQUIRE_STORAGE = "9.1.1";
    // new storage must not be 0x0
    string constant public OWNED_REQUIRE_NEW_STORAGE = "9.1.2";
    // new storage must be managed by me
    string constant public OWNED_REQUIRE_MANAGED = "9.1.3";
    */


    ///////////////////////////////////////
    // randomly generated to avoid key conflicts
    // but the fixed bytes32 would occupy to much space
    //bytes32 constant KEY_OWNER = 0x5a56315fb445b1a5ac55db632cdcaa16c04666bd7c0ca5f6a2808b5709b7b12c;
    // the special key of owner in the _storage
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
        //require(address(_newStorage) != address(0x0), OWNED_REQUIRE_NEW_STORAGE);
        //require(_newStorage.manager() == address(this), OWNED_REQUIRE_MANAGED);
        // no need to check. this func should be only used for testing.
        //require(address(_newStorage) != address(0x0) && _newStorage.manager() == address(this), "invalid new storage");

        _storage = _newStorage;
    }

    // owner
    event OwnerChanged(address _from, address _to);

    modifier onlyOwner {
        // no need to check storage because getOwner will do it.
        //require(msg.sender == getOwner(), OWNED_REQUIRE_ONLY_OWNER);
        require(msg.sender == getOwner(), "only owner");
        _;
    }

    function getOwner() view public returns (address) {
        //require(address(_storage) != address(0x0), OWNED_REQUIRE_STORAGE);
        // no need to check, 0x0.getAddress will fail anyway.
        //require(address(_storage) != address(0x0), "no storage");
        return _storage.getAddress(__OWNER__);
    }

    function changeOwner(address _newOwner) isRunning public onlyOwner {
        // no need to check storage because onlyOwner has done it.
        // code optimization to reduce code size
        emit OwnerChanged(getOwner(), _newOwner);
        _storage.setAddress(__OWNER__, _newOwner);
    }

    // let Manager can change owner too
    function setupOwner(address _newOwner) isRunning public onlyManager {
        // no need to check storage because getOwner will do it.
        address _oldOwner = getOwner();
        _storage.setAddress(__OWNER__, _newOwner);
        emit OwnerChanged(_oldOwner, _newOwner);
    }
}
