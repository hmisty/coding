pragma solidity >=0.4.22 <0.6.0;

import "Managed.sol";

contract KeyValueStorage is managed {

	/** 
	 * the general storage data structures
	 * 
	 * bytes32 <= keccak256(abi.encodePacked(A,B,C,....))
	 * which is just a raw hex
	 */
	mapping(bytes32 => address) _addressStorage;
	mapping(bytes32 => uint256) _uintStorage;
	mapping(bytes32 => bool) _boolStorage;

	/**** Storage Writer Setup **/
	// called by the Module
	constructor () public {
		manager = msg.sender; // manager should be the module address
	}

	/**** Get Methods ***********/

	function getAddress(bytes32 key) public view returns (address) {
		return _addressStorage[key];
	}

	function getUint(bytes32 key) public view returns (uint) {
		return _uintStorage[key];
	}

	function getBool(bytes32 key) public view returns (bool) {
		return _boolStorage[key];
	}

	/**** Set Methods ***********/

	function setAddress(bytes32 key, address value) isRunning onlyManager public {
		_addressStorage[key] = value;
	}

	function setUint(bytes32 key, uint value) isRunning onlyManager public {
		_uintStorage[key] = value;
	}

	function setBool(bytes32 key, bool value) isRunning onlyManager public {
		_boolStorage[key] = value;
	}

}
