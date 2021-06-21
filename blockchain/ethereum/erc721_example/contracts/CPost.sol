//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC721/ERC721.sol";

contract CPost is ERC721 {
    uint256 public totalSupply = 0; // total count of token

    mapping(uint256 => string) public _token_hash; // ipfs hash for token[id] content
    mapping(string => bool) _hash_exists; // for checking duplication
    mapping(uint256 => address) public _token_contributor; // for receiving rewards
    mapping(uint256 => uint256) _reward_sum; // total reward received for token[id]

    uint256 public _ratified_block_height; // the last block height being ratified and consolidated
    mapping(uint256 => bool) public _isReviewed; // if token[id] is Reviewed
    mapping(uint256 => bool) public _isRatified; // if token[id] is Ratified
    mapping(address => bool) public _isReviewer; // if address is Reviewer
    mapping(address => bool) public _isRatifier; // if address is Ratifier

    modifier onlyReviewer {
        require(_isReviewer[msg.sender], "not reviewer");
        _;
    }

    modifier onlyRatifier {
        require(_isRatifier[msg.sender], "not ratifier");
        _;
    }

    function changeReviewer(address person, bool x) public onlyRatifier {
        _isReviewer[person] = x;
    }

    function changeRatifier(address person, bool x) public onlyRatifier {
        _isRatifier[person] = x;
    }

    constructor() ERC721("CPost NFT", "CPost") {
        // the deployer will be reviewer and ratifier by default
        _isReviewer[msg.sender] = true;
        _isRatifier[msg.sender] = true;
    }

    // everyone can contribute a new post with the hash of its content
    function contribute(string memory hash) public returns (uint256) {
        require(_hash_exists[hash] == false, "already minted");
        _hash_exists[hash] = true;

        uint256 id = totalSupply;
        totalSupply += 1;

        _token_contributor[id] = msg.sender;
        _token_hash[id] = hash;
        _safeMint(msg.sender, id);

        return id;
    }

    // reviewer can pick content
    function pick(uint256 id, bool x) public onlyReviewer {
        require(_isReviewer[msg.sender], "not reviewer");
        _isReviewed[id] = x;
    }

    // ratifier can ratify
    function ratify(uint256 id, bool x) public onlyRatifier {
        require(_isRatifier[msg.sender], "not ratifier");
        _isRatified[id] = x;
    }

    // everyone can reward the contributor of a post
    function reward(uint256 id) public payable {
        require(_token_contributor[id] != address(0), "non-existent contributor");
        _reward_sum[id] += msg.value;
        Address.sendValue(payable(_token_contributor[id]), msg.value);
    }

}
