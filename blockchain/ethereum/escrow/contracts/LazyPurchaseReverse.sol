// SPDX-License-Identifier: Unlicensed
pragma solidity >=0.4.22 <0.7.0;

contract LazyPurchaseReverse {
    uint public price;
    address payable public seller;
    address payable public buyer;
    enum State { Inactive, Created, Locked, Soldout } 
    // The state variable has a default value of the first member, `State.created`
    State public state;

    // assume this is a thing that can be purchased
    constructor(uint _price) public {
        seller = msg.sender;
        price = _price;
    }

    modifier condition(bool _condition) {
        require(_condition);
        _;
    }

    modifier onlyBuyer() {
        require(
            msg.sender == buyer,
            "Only buyer can call this."
        );
        _;
    }

    modifier onlySeller() {
        require(
            msg.sender == seller,
            "Only seller can call this."
        );
        _;
    }

    modifier inState(State _state) {
        require(
            state == _state,
            "Invalid state."
        );
        _;
    }

    event OrderCreated();
    event OrderAborted();
    event PurchaseAccepted();
    event ItemReceived();

    /// Place order. anyone can do this.
    function placeOrder() public inState(State.Inactive) condition(msg.value == (2 * price)) payable {
        emit OrderCreated();
        buyer = msg.sender;
        state = State.Created;
    }

    /// Abort the purchase and reclaim the ether.
    /// Can only be called by the buyer before
    /// the contract is locked.
    function abortOrder() public onlyBuyer inState(State.Created) {
        emit OrderAborted();
        state = State.Inactive;
        buyer.transfer(2 * price); // refund buyer
    }

    /// Accept the purchase as seller.
    /// Transaction has to include `2 * value` ether.
    /// The ether will be locked until confirmReceived
    /// is called.
    function acceptPurchase() public onlySeller inState(State.Created) condition(msg.value == (2 * price)) payable {
        emit PurchaseAccepted();
        state = State.Locked;
    }

    /// Confirm that you (the buyer) received the item.
    /// This will release the locked ether.
    function confirmReceived() public onlyBuyer inState(State.Locked) {
        emit ItemReceived();
        // It is important to change the state first because
        // otherwise, the contracts called using `send` below
        // can call in again here.
        state = State.Soldout;

        // NOTE: This actually allows both the buyer and the seller to
        // block the refund - the withdraw pattern should be used.

        buyer.transfer(price); // buyer will get 1 back
        seller.transfer(address(this).balance); // seller will get all the rest, wich is 3
    }

    // returns the balance escrowed held by the contract
    function getBalance() public view returns(uint256) {
        return address(this).balance;
    }
}
