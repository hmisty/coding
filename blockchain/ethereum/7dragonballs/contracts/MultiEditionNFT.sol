//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC1155/ERC1155.sol";

contract MultiEditionNFT is ERC1155 {
    string public name = "Multi-edition NFT";
    string public symbol = "MENFT";

    uint256 public token_count = 0; // total count of token types
    uint256 public edition_count = 0; // total editions of all token types

    address _minter;
    mapping(uint256 => string) _token_hash; // ipfs hash for token[id] metadata
    mapping(string => bool) _hash_exists;

    modifier onlyMinter {
        require(_minter == msg.sender, "not minter");
        _;
    }

    constructor() ERC1155("") {
        _minter = msg.sender;
    }

    function changeMinter(address m) public onlyMinter {
        _minter = m;
    }

    function mint(string memory hash, uint256 amount) public onlyMinter returns (uint256) {
        require(_hash_exists[hash] == false, "already minted");
        _hash_exists[hash] = true;

        uint256 id = token_count;
        token_count += 1;
        edition_count += amount;

        _token_hash[id] = hash;
        _mint(msg.sender, id, amount, "");

        return id;
    }

    function _baseURI() internal pure returns (string memory) {
        return "ipfs://ipfs/";
    }

    function uri(uint256 tokenId) public view override returns (string memory) {
        require(tokenId < token_count, "nonexistent token");

        string memory baseURI = _baseURI();
        return bytes(baseURI).length > 0
            ? string(abi.encodePacked(baseURI, _token_hash[tokenId]))
            : '';
    }

}
