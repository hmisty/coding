//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC1155/ERC1155.sol";

contract SevenDragonBalls is ERC1155 {
    string public name = "7 Dragon Balls";
    string public symbol = "7DB";

    constructor() ERC1155("ipfs://ipfs/QmdEanCBF6fWhA3nfg23dyacTsGiBygHyXgxoCU4KfsQy3/7meta/{id}.json") {
        _mint(msg.sender, 0, 5000, "");
        _mint(msg.sender, 1, 3000, "");
        _mint(msg.sender, 2, 1000, "");
        _mint(msg.sender, 3,  500, "");
        _mint(msg.sender, 4,  250, "");
        _mint(msg.sender, 5,  200, "");
        _mint(msg.sender, 6,   50, "");
    }

}
