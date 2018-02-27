pragma solidity ^0.4.19;

// author: QY@startupmost, 2018-02-24
contract FlightAccidentInsurance {
    // who is delegated to report the accident
    // it can be changed with assignAccidentReporter() by current reporter
    // or with voteAccidentReporter() not yet implemented
    address public accidentReporter;
    
    // accident info
    //mapping(string => bool) accident; // string datedFlight == date + flightno
    
    // insured people
    struct InsuredPassengers {
        uint count;
        mapping(uint => address) person; // i => person
        mapping(uint => uint) fee; // i => fee == premium, in ETH. insured amount will be fee * rate
    }
    
    // all insured people indexed by datedFlight
    mapping(string => InsuredPassengers) datedFlights; // string datedFlight == date + flightno
    
    // rate = 1 / chance of accident * fee rate
    // referring to Ping'An 2 RMB => 6M RMB, we set rate to 300M
    // maybe it could allow to be changed with > 2/3 votes also?
    uint256 rate = 3000000;

    // constructor
    function FlightAccidentInsurance() public {
        accidentReporter = msg.sender;
    }

    // insure
    // leaving datedFlight string concatenated by the front-end
    function insure(string datedFlight) public payable {
        require(msg.value > 0);
        
        uint count = datedFlights[datedFlight].count;
        datedFlights[datedFlight].person[count] = msg.sender;
        datedFlights[datedFlight].fee[count] = msg.value;
        datedFlights[datedFlight].count = count + 1;
    }
    
    // reportAccident, only valid accidentReporter
    // auto pay out
    // if not enough balance, split pro rata
    function reportAccident(string datedFlight) public {
        require(msg.sender == accidentReporter);
        
        uint sumFee = 0;
        for (uint i = 0; i < datedFlights[datedFlight].count; i++) {
            sumFee += datedFlights[datedFlight].fee[i];
        }

        if (sumFee * rate < this.balance) {
            // auto pay out
            for (i = 0; i < datedFlights[datedFlight].count; i++) {
               address person = datedFlights[datedFlight].person[i];
               uint amount = datedFlights[datedFlight].fee[i] * rate;
               person.transfer(amount);
            }
        } else {
            // no enough balance, we pay out all pro rata
            for (i = 0; i < datedFlights[datedFlight].count; i++) {
               person = datedFlights[datedFlight].person[i];
               amount = datedFlights[datedFlight].fee[i] * this.balance / sumFee;
               person.transfer(amount);
            }
        }
    }
    
    // assign a new accident reporter
    // only current reporter can do this
    function assignAccidentReporter(address newReporter) public {
        require(msg.sender == accidentReporter);
        require(newReporter != address(0x0));
        
        accidentReporter = newReporter;
    }
    
    // vote a new accident reporter
    // not yet implemented
    
}
