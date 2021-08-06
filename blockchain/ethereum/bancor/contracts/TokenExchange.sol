// SPDX-License-Identifier: SEE LICENSE IN LICENSE
pragma solidity 0.6.12;

import "./SafeMath.sol";
import "./IBancorFormula.sol";
import "./BancorFormula.sol";
import "./IERC20.sol";

contract TokenExchange {
    using SafeMath for uint256;

    bool public initialized = false;

    IBancorFormula public formula;
    address public token; // token contract address
    address public owner;

    uint256 public reserveBalanceFixed; // = reserve balance + initialized reserve
    uint256 public tokenSupplyFixed;   // = max supply - (token balance - initialized reserve)
    uint32 public connWeight; // connect weight, i.e. reserve weight, in ppm (1-1000000)

    bytes4 private constant SELECTOR = bytes4(keccak256(bytes('transfer(address,uint256)')));

    /**
     * events
     */
    event TokenIssuance(uint256 targetAmount);

    /**
     * constuctor
     *
     * @param _token     address of the token contract
     */
    constructor(address _token) public {
        formula = new BancorFormula();
        token = _token;
        owner = msg.sender;
    }

    /**
     * modifier
     */
    modifier onlyOwner {
        require(msg.sender == owner, "Error: not owner");
        _;
    }

    /**
     * safeTransfer
     */
    function _safeTransfer(address _token, address _to, uint256 _value) private {
        (bool success, bytes memory data) = _token.call(abi.encodeWithSelector(SELECTOR, _to, _value));
        require(success && (data.length == 0 || abi.decode(data, (bool))), "Error: failed to transfer");
    }

    /**
     * initialize parameters
     *
     * @param _initialSupply    initial token supply (virtual)
     * @param _initialReserve   initial reserve token balance (virtual)
     * @param _reserveWeight    reserve weight, represented in ppm (1-1000000)
     *
     * then the initial spot price will be 
     * _initialReserve/_initialSupply * 1000000/_reserveWeight
     */
    function initialize(uint256 _initialSupply, uint256 _initialReserve, uint32 _reserveWeight) public onlyOwner {
        require(initialized == false, "Error: already initialized");

        reserveBalanceFixed = _initialReserve;
        tokenSupplyFixed = _initialSupply;
        connWeight = _reserveWeight;

        initialized = true;
    }

    /**
     * purchase a certain target amount of tokens by paying in,
     * the issued tokens will be put back into the caller's wallet
     *
     */
    function purchase() public payable {
        require(msg.value > 0, "Error: too few paid");
        uint256 amount = msg.value;
        uint256 targetAmount = formula.purchaseTargetAmount(tokenSupplyFixed, reserveBalanceFixed, connWeight, amount);

        // update
        tokenSupplyFixed += targetAmount;
        reserveBalanceFixed += amount;

        // tranfer token to the buyer
        require(IERC20(token).balanceOf(address(this)) >= targetAmount, "Error: not enough tokens");
        _safeTransfer(token, msg.sender, targetAmount);
        emit TokenIssuance(targetAmount);
    }

    /**
     * get real balance of tokens
     */
    function getTokenBalance() public view returns(uint256) {
        return IERC20(token).balanceOf(address(this));
    }

    /**
     * get real balance of reserve
     */
    function getReserveBalance() public view returns(uint256) {
        return address(this).balance;
    }

    /**
     * withdraw all remaining tokens
     */
    function withdrawTokens() public onlyOwner {
        uint256 amount = getTokenBalance();
        _safeTransfer(token, owner, amount);
    }

    /**
     * withdraw reserve
     */
    function withdrawReserve() public onlyOwner {
        uint256 amount = getReserveBalance();
        (bool success,) = owner.call{value: amount}("");
        require(success, "Error: unable to send");
    }

}
