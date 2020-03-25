pragma solidity >=0.4.22 <0.5.0;

import "./Owned.sol";

/**
 * The thin contract for module implementations to inherit.
 * Keep this slim, for the module implementations are usually big and easy to
 * be out of gas when deployed.
 *
 * The owner is stored in the key value storage with a special key
 *
 */
contract ModuleImpl is owned {
    
    // just for being inherited

}
