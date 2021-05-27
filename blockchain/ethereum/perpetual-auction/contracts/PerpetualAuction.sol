//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC721/IERC721.sol";
import "@openzeppelin/contracts/token/ERC1155/IERC1155.sol";
import "@openzeppelin/contracts/utils/introspection/ERC165.sol";
import "@openzeppelin/contracts/utils/math/SafeMath.sol";
import "@openzeppelin/contracts/utils/Address.sol";

contract PerpetualAuction {
    using SafeMath for uint;

    address payable public feeCollector;
    uint public feeRate = 25; // 25 thousandth, i.e. 2.5% contract fee

    enum State { Inactive, Active }
    enum Type { ERC721, ERC1155 }

    struct Item {
        uint index;
        address token; // NFT contract address
        uint tokenId;
        Type tokenType; // ERC-721 or ERC-1155
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

    constructor(address payable _to) {
        feeCollector = _to == address(0) ? payable(msg.sender) : _to;
    }

    function create(address _token, uint _id, uint _amount, uint _price, uint _markup) public {

        uint created_amount;
        Type created_type;

        if (IERC165(_token).supportsInterface(type(IERC721).interfaceId)) { // is ERC-721
            IERC721(_token).safeTransferFrom(msg.sender, address(this), _id);
            created_amount = 1;
            created_type = Type.ERC721;
        } else if (IERC165(_token).supportsInterface(type(IERC1155).interfaceId)) { // is ERC-1155
            IERC1155(_token).safeTransferFrom(msg.sender, address(this), _id, _amount, "");
            created_amount = _amount;
            created_type = Type.ERC1155;
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

        // TODO emit an event?
    }

    function bid(uint _index) payable public {
        uint lastPrice = items[_index].lastPrice;
        uint minPayment = lastPrice.add(lastPrice.mul(items[_index].minMarkup).div(1000));
        require(msg.value >= minPayment, "Bidding price too low.");

        uint fee = msg.value.mul(feeRate).div(1000);
        Address.sendValue(feeCollector, fee);
        Address.sendValue(items[_index].lastOwner, msg.value.sub(fee));

        items[_index].lastPrice = msg.value;
        items[_index].lastOwner = payable(msg.sender);

        // TODO emit an event?
    }

}
