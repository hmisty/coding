pragma solidity >=0.4.22 <0.6.0;

import "./SafeMath.sol";
import "./upgradable/Upgradable.sol";

/**
 * The DAOImpl contract soft upgradable that contains only the business logic.
 * It can be painlessly changed without upgrading the main contract ModuleA.
 */
contract DAOImpl is upgradable {
    using SafeMath for uint256;

    ///////////////////////////////////////////////////
    //   feel free to implement anything below       //
		//   all are able to be soft-upgraded            //
    ///////////////////////////////////////////////////

    // reveal the version tag. this is not mandatory either.
    function getVersionTag() pure public returns (string memory) {
        return "0.0.2";
    }

    /**
     * all the encoded keys used in the K-V storage.
     */
    bytes32 constant public KEY_ENABLED = keccak256(abi.encodePacked("enabled"));
    bytes32 constant public KEY_NUM_MEMBERS = keccak256(abi.encodePacked("number-of-memebers"));

    /**
     * Only owner can enable it.
     */
    function enable() isRunning payable public onlyOwner {
        require (msg.value >= 100000);
        _storage.setBool(KEY_ENABLED, true);
    }

    /**
     * bla bla
     */
    function setNumberOfMembers(uint256 num) isRunning public {
        _storage.setUint(KEY_NUM_MEMBERS, num);
    }

    /**
     * bla bla
     */
    function getNumberOfMembers() view public returns (uint256) {
        return _storage.getUint(KEY_NUM_MEMBERS);
    }

}
