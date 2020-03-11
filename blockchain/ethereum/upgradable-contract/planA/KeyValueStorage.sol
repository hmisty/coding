pragma solidity >=0.4.22 <0.6.0;

import "Managed.sol";

contract KeyValueStorage is managed {
  
  /** the general storage data structures **/
  
  mapping(string => address) _addressStorage;
  mapping(string => uint256) _uintStorage;
  mapping(string => bool) _boolStorage;

  /**** Storage Writer Setup **/
  // called by the Module
  constructor () public {
      manager = msg.sender; // manager should be the module address
  }

  /**** Get Methods ***********/

  function getAddress(string key) public view returns (address) {
      return _addressStorage[key];
  }

  function getUint(string key) public view returns (uint) {
      return _uintStorage[key];
  }

  function getBool(string key) public view returns (bool) {
      return _boolStorage[key];
  }

  /**** Set Methods ***********/

  function setAddress(string key, address value) isRunning onlyManager public {
    _addressStorage[key] = value;
  }

  function setUint(string key, uint value) isRunning onlyManager public {
      _uintStorage[key] = value;
  }

  function setBool(string key, bool value) isRunning onlyManager public {
      _boolStorage[key] = value;
  }

}
