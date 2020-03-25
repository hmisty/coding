pragma solidity >=0.4.22 <0.5.0;

import "./upgradable/ModuleImpl.sol";

import "./lib/SafeMath.sol";
import { Membership } from "./lib/Membership.sol";

/**
 * The AppImpl contract soft upgradable that contains only the business logic.
 * It can be painlessly changed without upgrading the main contract ModuleA.
 */
contract AppImpl is ModuleImpl {
    using SafeMath for uint256;

    ///////////////////////////////////////////////////
    //   feel free to implement anything below       //
	//   all are able to be soft-upgraded            //
    ///////////////////////////////////////////////////

    /**
     * reveals the version tag. this is not mandatory either.
     */
    function getVersionTag() pure public returns (string memory) {
        return "0.0.1";
    }

    /**
     * returns the balance.
     */
    function getBalance() view public returns (uint256) {
        return address(this).balance;
    }


    /**
     * all the encoded keys used in the K-V storage.
     */
    bytes32 constant public KEY_ENABLED = keccak256(abi.encodePacked("enabled"));
    bytes32 constant public KEY_NUM_MEMBERS = keccak256(abi.encodePacked("number-of-memebers"));

    /**
     * Only owner can enable it.
     * TODO: check if caller is owner.
     */
    function enable() payable public {
        require (msg.value >= 100000);
        _storage.setBool(KEY_ENABLED, true);
    }

    /**
     * bla bla
     */
    function setNumberOfMembers(uint256 num) public {
        Membership.setNumberOfMembers(_storage, num);
    }

    /**
     * bla bla
     */
    function getNumberOfMembers() view public returns (uint256) {
        return Membership.getNumberOfMembers(_storage);
    }

    /**
     * is member?
     */
    function isMember(address _member) public view returns (bool){
        return Membership.isMember(_storage, _member);
    }

    /**
     * is admin?
     */
    function isAdmin(address _admin) public view returns (bool){
        return Membership.isAdmin(_storage, _admin);
    }

}
