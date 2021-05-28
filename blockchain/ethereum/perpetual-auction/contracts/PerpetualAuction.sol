//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC721/IERC721.sol";
import "@openzeppelin/contracts/token/ERC721/utils/ERC721Holder.sol";
import "@openzeppelin/contracts/token/ERC1155/IERC1155.sol";
import "@openzeppelin/contracts/token/ERC1155/utils/ERC1155Holder.sol";
import "@openzeppelin/contracts/utils/introspection/ERC165.sol";
import "@openzeppelin/contracts/utils/math/SafeMath.sol";
import "@openzeppelin/contracts/utils/Address.sol";

contract PerpetualAuction is ERC721Holder, ERC1155Holder {
    using SafeMath for uint;

    address payable public feeCollector;
    uint public feeRate = 25; // 25 thousandth, i.e. 2.5% contract fee

    enum State { Inactive, Active }
    enum TokenType { ERC721, ERC1155 }

    struct Item {
        uint index;
        address token; // NFT contract address
        uint tokenId;
        TokenType tokenType; // ERC-721 or ERC-1155
        uint amount; // only for ERC-1155
        uint lastPrice;
        address payable lastOwner;
        uint firstPrice;
        uint minMarkup; // e.g. 50 means 50 thousandth, i.e. +5% markup at least
        State state;
    }
    // royalty is not defined here, but within the NFT ?

    mapping(uint=>Item) public items;
    uint public totalSupply;

    event Created(address creator, uint index);
    event Bid(address bidder, uint index);
    event Withdrawn(address taker, uint index);

    constructor(address payable _to) {
        feeCollector = _to == address(0) ? payable(msg.sender) : _to;
    }

    function create(address _token, uint _id, uint _amount, uint _price, uint _markup) public {

        uint created_amount;
        TokenType created_type;

        if (IERC165(_token).supportsInterface(type(IERC721).interfaceId)) { // is ERC-721
            IERC721(_token).safeTransferFrom(msg.sender, address(this), _id); // non-safe
            created_amount = 1;
            created_type = TokenType.ERC721;
        } else if (IERC165(_token).supportsInterface(type(IERC1155).interfaceId)) { // is ERC-1155
            IERC1155(_token).safeTransferFrom(msg.sender, address(this), _id, _amount, ""); // non-safe
            created_amount = _amount;
            created_type = TokenType.ERC1155;
        } else {
            revert();
        }

        Item memory item = Item({
            index: totalSupply,
            token: _token,
            tokenId: _id,
            tokenType: created_type,
            amount: created_amount,
            lastPrice: _price,
            lastOwner: payable(msg.sender),
            firstPrice: _price,
            minMarkup: _markup,
            state: State.Active
        });

        items[item.index] = item;
        totalSupply++;

        emit Created(msg.sender, item.index);
    }

    function bid(uint _index) payable public {
        require(items[_index].state == State.Active, "Inactive auction.");

        uint lastPrice = items[_index].lastPrice;
        uint minPayment = lastPrice.add(lastPrice.mul(items[_index].minMarkup).div(1000));
        require(msg.value >= minPayment, "Bidding price too low.");

        uint fee = msg.value.mul(feeRate).div(1000);
        Address.sendValue(feeCollector, fee);
        Address.sendValue(items[_index].lastOwner, msg.value.sub(fee));

        items[_index].lastPrice = msg.value;
        items[_index].lastOwner = payable(msg.sender);

        emit Bid(msg.sender, _index);
    }

    function withdraw(uint _index) public {
        require(msg.sender == items[_index].lastOwner, "Only last owner can withdraw.");
        
        address _token = items[_index].token;
        uint _id = items[_index].tokenId;
        TokenType _type = items[_index].tokenType;
        uint _amount = items[_index].amount;

        if (_type == TokenType.ERC721) { // is ERC-721
            IERC721(_token).safeTransferFrom(address(this), msg.sender, _id); // non-safe
        } else if (_type == TokenType.ERC1155) { // is ERC-1155
            IERC1155(_token).safeTransferFrom(address(this), msg.sender, _id, _amount, ""); // non-safe
        } else {
            revert();
        }

        items[_index].state = State.Inactive; // stop the auction

        emit Withdrawn(msg.sender, _index);
    }

}
