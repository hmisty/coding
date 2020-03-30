pragma solidity >=0.4.22 <0.5.0;

import "./Owned.sol";

/**
 * The Module contract that is upgradable (i.e. relocated to a new contract).
 *
 * Data is permanently stored in an external storage contract.
 * Fund will be transferred to the newly upgraded contract.
 */
contract Module is owned {

    ///////////////////////////////////////
    // the special key of implementation in _storage
    bytes32 internal constant __IMPL__ = sha256("__impl__");

    ///////////////////////////////////////
    //       upgrade functions           //
    ///////////////////////////////////////
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

    /**
     * Step 2. Setup the storage for the new module.
     *
     * Only manager can initialize.
     *
     * do not use isRunning, for 1) manager can start; 2) convenient for
     * manager to maintain the system while it is not runnning
     */
    function setupStorage(address _legacyStorage) onlyManager public {
        // initialize storage
        if (_legacyStorage != address(0x0)) {
            // use legacy storage if having one
            _storage = KeyValueStorage(_legacyStorage);
            // still no access, need to wait for _legacyStorage.upgradeTo() done.
        } else if (address(_storage) == address(0x0)) {
            // otherwise create a new storage
            // manager of the storage is Module
            _storage = new KeyValueStorage();
        }
    }

    /**
     * Step 3. Transfer the fund from the old module to the new module.
     *
     * Only manager can upgrade.
     *
     * do not use isRunning, for 1) manager can start; 2) convenient for
     * manager to maintain the system while it is not runnning
     */
    function upgradeTo(address _newModule) onlyManager public {
        // transfer total fund to new module
        bool success = Module(_newModule).receiveFund.value(getBalance())();
        // we do not use require(success) for less deployment gas
        if (success) {
            // transfer storage access
            _storage.changeManager(_newModule);

            // deprecate the storage of the old module for avoiding misuse.
            _storage = KeyValueStorage(address(0x0));

            // stop running
            setRunning(false);
        }
    }

    /**
     * for checking the balance of the module.
     */
    function getBalance() view public returns (uint256) {
        return address(this).balance;
    }

    /**
     * for receiving upgrading fund transfer.
     */
    event FundReceived(address _sender, uint256 _amount);

    function receiveFund() external payable returns (bool) {
        emit FundReceived(msg.sender, msg.value);
        return true;
    }

    ///////////////////////////////////////////////////
    //           use delegated implementation        //
    ///////////////////////////////////////////////////

    event ImplementationChanged(address _oldImpl, address _newImpl);

    // the implementation instance, as well as a function implementation()
    //address public implementation = address(0x0);
    // WARNING: DO NOT INTRODUCE MORE CONTRACT VARIABLES
    
    /**
    * returns current impl
    */
    function getImplementation() public view returns (address) {
        // no need to check _storage, 0x0.getAddress will fail anyway.
        return _storage.getAddress(__IMPL__);
    }

    /**
     * with this you can enjoy painless "implementation upgrade" only
     * without upgrading the full module :)
     *
     * do not use isRunning, for 1) manager can start; 2) convenient for
     * manager to maintain the system while it is not runnning
     */
    function changeImplementation(address _newImpl) public onlyManager {
        address _oldImpl = getImplementation();
        _storage.setAddress(__IMPL__, _newImpl);
        emit ImplementationChanged(_oldImpl, _newImpl);
    }

    /**
     * the assembly trick to delegate any function call.
     */
    function () isRunning payable external {
        address _impl = getImplementation();
        require(_impl != address(0), "no impl");

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
