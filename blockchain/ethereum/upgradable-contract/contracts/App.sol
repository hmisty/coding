pragma solidity >=0.4.22 <0.5.0;

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
    
}
