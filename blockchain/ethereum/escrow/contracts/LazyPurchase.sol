// SPDX-License-Identifier: Unlicensed
pragma solidity >=0.4.22 <0.7.0;

interface IWETH {
    function balanceOf(address owner) external view returns (uint);
    function approve(address, uint) external returns (bool);
    function deposit() external payable;
    function transfer(address, uint) external returns (bool);
    function transferFrom(address src, address dst, uint wad) external returns (bool);
    function withdraw(uint) external;
}

contract LazyPurchase {
    IWETH public weth; // the WETH contract deployed
    uint public price;
    address payable public seller;
    address payable public buyer;
    enum State { Created, Locked, Inactive } 
    // The state variable has a default value of the first member, `State.created`
    State public state;

    // Ensure that `msg.value` is an even number.
    // Division will truncate if it is an odd number.
    // Check via multiplication that it wasn't an odd number.
    constructor(address _WETH) public {
        seller = msg.sender;
        weth = IWETH(_WETH);
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

    event Aborted();
    event PurchaseConfirmed();
    event ItemReceived();

    /// Create the sale.
    function create(uint _price) public onlySeller
    {
        price = _price;
        require((2 * price) <= weth.balanceOf(seller), "Error: Seller WETH < 2 * price");

        state = State.Created;
    }

    /// Abort the purchase and reclaim the ether.
    /// Can only be called by the seller before
    /// the contract is locked.
    function abort()
        public
        onlySeller
        inState(State.Created)
    {
        emit Aborted();
        state = State.Inactive;
        //no need to refund the seller because of lazy lock-up
    }

    /// Confirm the purchase as buyer.
    /// Transaction has to include `2 * value` ether.
    /// The ether will be locked until confirmReceived
    /// is called.
    function confirmPurchase()
        public
        inState(State.Created)
        condition(msg.value == (2 * price))
        payable
    {
        emit PurchaseConfirmed();
        buyer = msg.sender;
        state = State.Locked;

        weth.transferFrom(seller, address(this), msg.value);
        // hold seller's WETH
        // and buyer's ETH
    }

    /// Confirm that you (the buyer) received the item.
    /// This will release the locked ether.
    function confirmReceived()
        public
        onlyBuyer
        inState(State.Locked)
    {
        emit ItemReceived();
        // It is important to change the state first because
        // otherwise, the contracts called using `send` below
        // can call in again here.
        state = State.Inactive;

        // NOTE: This actually allows both the buyer and the seller to
        // block the refund - the withdraw pattern should be used.

        buyer.transfer(price); // refund buyer
        seller.transfer(address(this).balance); // give the price
        weth.transfer(seller, weth.balanceOf(address(this))); // refund seller
    }

    // returns the balance escrowed held by the contract
    function balance()
        public
        view
        returns(uint256)
    {
        return address(this).balance;
    }
}
