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
  
    /**
     * Step 1. Deploy the newest version of the module, with an owner of either:
     * (1) a new owner who is creating a new module.
     * (2) the owner of the legacy module.
     *
     * NOTICE: Let only manager deploy!
     * 
     */
    constructor() public {
        manager = msg.sender;
    }
    
    ///////////////////////////////////////////////////
    //        free to implement anything below       //
    ///////////////////////////////////////////////////

    /**
     * Only owner can enable it.
     */
    function enableModuleA() isRunning payable public onlyOwner {
        require (msg.value >= 100000);
        KeyValueStorage(_storage).setBool("moduleA-enabled", true);
    }

    /**
     * bla bla
     */ 
    function setNumberOfMembers(uint256 num) isRunning public {
        KeyValueStorage(_storage).setUint("number-of-members", num);
    }
    
    /**
     * bla bla
     */    
    function getNumberOfMembers() view public returns (uint256) {
        return KeyValueStorage(_storage).getUint("number-of-members");
    }
    
}
