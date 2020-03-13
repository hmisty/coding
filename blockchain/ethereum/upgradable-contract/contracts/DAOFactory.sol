pragma solidity >=0.4.22 <0.6.0;

import "./ModuleFactory.sol";
import "./DAO.sol";

/**
 * Maintains the registries of contracts for verification use.
 */
contract DAOFactory is ModuleFactory {

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
        return address(new DAO());
    }

}
