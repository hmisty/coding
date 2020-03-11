pragma solidity >=0.4.22 <0.6.0;

import "SafeMath.sol";
import "Upgradable.sol";

/**
 * The ModuleA contract that is upgradable (i.e. relocated to a new contract).
 * 
 * Data is permanently stored in an external storage contract.
 * Fund will be transferred to the newly upgraded contract.
 */
contract ModuleA is upgradable {
    using SafeMath for uint256;
    
    // the version meta data.
    uint256 constant public version = 0x03;
    
    /**
     * Step 1. Deploy the newest version of the module, with an owner of either:
     * (1) a new owner who is creating a new module.
     * (2) the owner of the legacy module.
     *
     * NOTICE: Let only the factory deploy!
     * 
     */
    constructor() public {
        manager = msg.sender; // this should be the factory address
    }
    
    ///////////////////////////////////////////////////
    //        free to implement anything below       //
    ///////////////////////////////////////////////////
    
    /**
     * all the encoded keys used in the K-V storage.
     */
    bytes32 constant public KEY_ENABLED = keccak256(abi.encodePacked("enabled"));
    bytes32 constant public KEY_NUM_MEMBERS = keccak256(abi.encodePacked("number-of-memebers"));

    /**
     * Only owner can enable it.
     */
    function enableModuleA() isRunning payable public onlyOwner {
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
