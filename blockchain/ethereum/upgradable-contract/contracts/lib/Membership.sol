pragma solidity >=0.4.22 <0.5.0;

import "../upgradable/KeyValueStorage.sol";

/**
 * The library of membership related functions.
 */
library Membership {

    /**
     * all the encoded keys used in the K-V storage.
     */
    bytes32 constant public KEY_NUM_MEMBERS = keccak256(abi.encodePacked("number-of-memebers"));

    /**
     * set number of members
     */
    function setNumberOfMembers(KeyValueStorage _storage, uint256 num) public {
        _storage.setUint(KEY_NUM_MEMBERS, num);
    }

    /**
     * returns number of members
     */
    function getNumberOfMembers(KeyValueStorage _storage) view public returns (uint256) {
        return _storage.getUint(KEY_NUM_MEMBERS);
    }

    /**
     * is member?
     */
    function isMember(KeyValueStorage _storage, address _member) public view returns (bool){
        bytes32 key = keccak256(abi.encodePacked("membership", _member));
        return _storage.getBool(key);
    }

    /**
     * is admin?
     */
    function isAdmin(KeyValueStorage _storage, address _admin) public view returns (bool){
        bytes32 key = keccak256(abi.encodePacked("admin", _admin));
        bool _isAdmin = isMember(_storage, _admin) && _storage.getBool(key);
        return _isAdmin;
    }

}
