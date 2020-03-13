pragma solidity >=0.4.22 <0.6.0;

import "./Upgradable.sol";

/**
 * The ModuleA contract that is upgradable (i.e. relocated to a new contract).
 * 
 * Data is permanently stored in an external storage contract.
 * Fund will be transferred to the newly upgraded contract.
 */
contract ModuleA is upgradable {

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
    //   feel free to implement anything below       //
    ///////////////////////////////////////////////////
    
    /**
     * all the encoded keys used in the K-V storage.
     */
    //bytes32 constant public KEY_ENABLED = keccak256(abi.encodePacked("enabled"));
    //bytes32 constant public KEY_NUM_MEMBERS = keccak256(abi.encodePacked("number-of-memebers"));

    /**
     * Only owner can enable it.
     */
    //function enableModuleA() isRunning payable public onlyOwner {
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
    
    ///////////////////////////////////////////////////
    //        or use a delegated implementation      //
    ///////////////////////////////////////////////////
    
    event ImplementationChanged(address _oldImpl, address _newImpl);
    
    // the implementation instance
    address public _implementation = address(0x0);
    
    function implementation() view public returns (address) {
        return _implementation;
    }
    
    /**
     * with this you can enjoy painless "implementation upgrade" only
     * without upgrading the full module :)
     */
    function changeImplementation(address _newImpl) isRunning public onlyManager {
        require(_implementation != _newImpl, "already using this implementation.");
        address _oldImpl = _implementation;
        _implementation = _newImpl;
        emit ImplementationChanged(_oldImpl, _newImpl);
    }

    /**
     * the assembly trick to delegate any function call.
     */
    function () isRunning payable external {
        address _impl = implementation();
        require(_impl != address(0));

        assembly {
            let ptr := mload(0x40)
            calldatacopy(ptr, 0, calldatasize)
            let result := delegatecall(gas, _impl, ptr, calldatasize, 0, 0)
            mstore(0x40, add(ptr, returndatasize))
            returndatacopy(ptr, 0, returndatasize)
            switch result
                case 0 { revert(ptr, returndatasize) }
                default { return(ptr, returndatasize) }
        }
    }

}
