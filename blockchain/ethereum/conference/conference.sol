pragma solidity ^0.4.19;

//deployed at https://ropsten.etherscan.io/address/0x9E2C0022Fa03AF3A708cB383081446E2E52c806A
contract Conference {
  address public organizer;
  mapping (address => uint) public registrantsPaid;
  uint public numRegistrants;
  uint public quota;

  event Deposit(address _from, uint _amount);  // so you can log these events
  event Refund(address _to, uint _amount); 

  function Conference() public { // Constructor
    organizer = msg.sender;
    quota = 500;
    numRegistrants = 0;
  }
  
  function getBalance() public view returns (uint) {
      return this.balance;
  }
  
  function buyTicket() public payable returns (bool success) {
    if (quota > 0 //still having quota
        && registrantsPaid[msg.sender] == 0 //1 addr 1 ticket only
        && msg.value > 0 //price paid must > 0
    ) {
        registrantsPaid[msg.sender] = msg.value;
        numRegistrants++;
        quota--;
        Deposit(msg.sender, msg.value);
        return true;
    } else {
        msg.sender.transfer(msg.value); //send back his ether
        return false;
    }
  }
  
  function changeQuota(uint newquota) public {
    if (msg.sender != organizer) { return; }
    quota = newquota;
  }
  
  function getMyTicketPrice() public view returns (uint) {
      return registrantsPaid[msg.sender];
  }
  
  function refundTicket(address recipient) public {
    if (msg.sender != organizer) { return; } //only organizer can do this
    uint amount = registrantsPaid[recipient];
    if (amount > 0 //not yet refunded
        && this.balance >= amount) { 
        recipient.transfer(amount);
        registrantsPaid[recipient] = 0;
        numRegistrants--;
        quota++;
        Refund(recipient, amount);
    }
  }
  
  function destroy() public { // so funds not locked in contract forever
    if (msg.sender == organizer) { 
      selfdestruct(organizer); // send funds to organizer
    }
  }
  
}
