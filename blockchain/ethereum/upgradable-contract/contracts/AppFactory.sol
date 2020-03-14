pragma solidity >=0.4.22 <0.5.0;

import "./upgradable/ModuleFactory.sol";
import "./App.sol";

/**
 * Maintains the registries of contracts for verification use.
 */
contract AppFactory is ModuleFactory {

    /**
     * NOTICE: Let only the system manager deploy!
     *
     */

    /**
     * Implement the abstract function.
     * returns a new instance of Module.
     */ 
    // FIXME 0.4.24 compatible, use address payable for 0.5.0 
    function newModule() public returns (address) {
        return address(new App());
    }

}
