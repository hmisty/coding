//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC1155/ERC1155.sol";

contract SevenDragonBalls is ERC1155 {

    string _uriPrefix; // prefix for all URIs

    constructor() ERC1155("") {
        _uriPrefix = "ipfs://ipfs/QmPKku7yu7tHv7JyjhE32b3GJRgTyvdBTZ82nMU1CQ22Wi/7dbmeta/";

        uint256[] memory ids = new uint256[](7);
        ids[0] = 0; ids[1] = 1; ids[2] = 2; ids[3] = 3; ids[4] = 4; ids[5] = 5; ids[6] = 6;
        uint256[] memory counts = new uint256[](7);
        counts[0] = 5000; counts[1] = 3000; counts[2] = 1000; counts[3] = 500; counts[4] = 250; counts[5] = 200; counts[6] = 50;

        _mintBatch(msg.sender, ids, counts, "");
    }

    function uri(uint256 _id) public view override returns (string memory) {
        return string(abi.encodePacked(_uriPrefix, _id, ".json"));
    }

}
