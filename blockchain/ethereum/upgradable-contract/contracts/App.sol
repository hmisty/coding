pragma solidity >=0.4.22 <0.6.0;

import "./upgradable/Module.sol";

/**
 * The App contract that is (hard) upgradable (i.e. relocated to a new contract).
 * 
 * Data is permanently stored in an external storage contract.
 * Fund will be transferred to the newly upgraded contract.
 */
contract App is Module {

    /**
     * safe to leave this blank.
     * anything here needs hard upgrade if changed.
     *
     * NOTICE: Let only the factory deploy!
     *
     */

    ///////////////////////////////////////////////////
    //     anything below can be hard-upgraded       //
    ///////////////////////////////////////////////////
    
    /**
     * all the encoded keys used in the K-V storage.
     */
    //bytes32 constant public KEY_ENABLED = keccak256(abi.encodePacked("enabled"));
    //bytes32 constant public KEY_NUM_MEMBERS = keccak256(abi.encodePacked("number-of-memebers"));

    /**
     * Only owner can enable it.
     */
    //function enableModule() isRunning payable public onlyOwner {
    //    require (msg.value >= 100000);
    //    _storage.setBool(KEY_ENABLED, true);
    //}

    /**
     * bla bla
     */ 
    //function setNumberOfMembers(uint256 num) isRunning public {
    //    _storage.setUint(KEY_NUM_MEMBERS, num);
    //}
    
    /**
     * bla bla
     */    
    //function getNumberOfMembers() view public returns (uint256) {
    //    return _storage.getUint(KEY_NUM_MEMBERS);
    //}
    
}
