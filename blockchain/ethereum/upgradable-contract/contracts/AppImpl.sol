pragma solidity >=0.4.22 <0.5.0;

import "./SafeMath.sol";
import "./upgradable/Owned.sol";

/**
 * The AppImpl contract soft upgradable that contains only the business logic.
 * It can be painlessly changed without upgrading the main contract ModuleA.
 */
contract AppImpl is owned {
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
        _storage.setUint(KEY_NUM_MEMBERS, num);
    }

    /**
     * bla bla
     */
    function getNumberOfMembers() view public returns (uint256) {
        return _storage.getUint(KEY_NUM_MEMBERS);
    }

    /**
     * is member?
     */
    function isMember(address _member) public view returns (bool){
        bytes32 card1BlockKey = keccak256(abi.encodePacked("membersCardBlock", uint256(1), _member));
        bytes32 card2BlockKey = keccak256(abi.encodePacked("membersCardBlock", uint256(2), _member));

        uint256 card1Block = _storage.getUint(card1BlockKey);
        uint256 card2Block = _storage.getUint(card2BlockKey);
        uint256 curBlock = block.number;

        bool _isMember = (card1Block >= curBlock || card2Block >= curBlock);
        return _isMember;
    }

    /**
     * is admin?
     */
    function isAdmin(address _admin) public view returns (bool){
        if (isMember(_admin) == false) {
            return false;
        }

        bytes32 _key = keccak256(abi.encodePacked("admin", _admin));
        return _storage.getBool(_key);
    }

}
