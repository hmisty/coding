//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC721/ERC721.sol";

contract CryptoScientists is ERC721 {
    uint256 public token_count = 0; // total count of token

    address _minter;
    mapping(uint256 => string) _token_hash; // ipfs hash for token[id]
    mapping(string => bool) _hash_exists;

    modifier onlyMinter {
        require(_minter == msg.sender, "not minter");
        _;
    }

    constructor() ERC721("Crypto Scientists NFT", "CSNFT") {
        _minter = msg.sender;
    }

    function changeMinter(address m) public onlyMinter {
        _minter = m;
    }

    function mint(string memory hash) public onlyMinter returns (uint256) {
        require(_hash_exists[hash] == false, "already minted");
        _hash_exists[hash] = true;

        uint256 id = token_count;
        token_count += 1;

        _token_hash[id] = hash;
        _safeMint(msg.sender, id);

        return id;
    }

    function _baseURI() internal pure override returns (string memory) {
        return "ipfs://ipfs/";
    }

    function tokenURI(uint256 tokenId) public view override returns (string memory) {
        require(_exists(tokenId), "nonexistent token");

        string memory baseURI = _baseURI();
        return bytes(baseURI).length > 0
            ? string(abi.encodePacked(baseURI, _token_hash[tokenId]))
            : '';
    }



}
