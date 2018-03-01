pragma solidity ^0.4.19;

// author: QY@startupmost, 2018-02-24
contract FlightAccidentInsurance {
    // who is delegated to report the accident
    // it can be changed with assignAccidentReporter() by current reporter
    // or with voteAccidentReporter() not yet implemented
    address public accidentReporter;
    
    // a policy, an insurance slip
    struct Policy {
        address person; //policy holder
        string flight; //date+flightno
        mapping(string => bool) isFlight;
        uint premium; //insurance fee
        uint amount; //insured amount
        uint actualAmount; //acutalAmount <= amount
        bool paid; //claimed and paid
    }
    
    // accident datedFlight => true
    struct Accident {
        bool confirmed;
        uint count; // number of slips to be paid
        uint sumAmount; // total amount to pay out
        mapping(uint => Policy) affectedPolicies;
    }
    
    mapping(string => Accident) accidents; // indexed by date+flightno

    // a person's all insurance policies
    struct PersonPolicies {
        uint count;
        mapping(uint => Policy) policies;
    }
    
    mapping(address => PersonPolicies) personPolicies; //indexed by person
    
    // all passengers of a date+flightno
    struct FlightPassengers {
        uint count;
        mapping(uint => address) person; //for iterating
        mapping(address => bool) isPassenger; //for checking
    }
    
    mapping(string => FlightPassengers) flightPassengers; //indexed by flight
    
    // rate = 1 / chance of accident * fee rate
    // referring to Ping'An 2 RMB => 6M RMB, we set rate to 300M
    // maybe it could allow to be changed with > 2/3 votes also?
    uint256 rate = 3000000;

    // constructor
    function FlightAccidentInsurance() public {
        accidentReporter = msg.sender;
    }

    // insure
    // leaving date+flightno string concatenated by the front-end
    function insure(string flight) public payable {
        require(msg.value > 0);

        address person = msg.sender;

        // not allow to insure one trip twice or more
        require(flightPassengers[flight].isPassenger[person] == false);
        
        // on board
        flightPassengers[flight].isPassenger[person] = true;
        uint c = flightPassengers[flight].count;
        flightPassengers[flight].person[c] = person;
        flightPassengers[flight].count += 1;
            
        // insure a trip
        uint count = personPolicies[person].count;
        personPolicies[person].policies[count].person = person;
        personPolicies[person].policies[count].flight = flight;
        personPolicies[person].policies[count].isFlight[flight] = true;
        personPolicies[person].policies[count].premium = msg.value;
        personPolicies[person].policies[count].amount
            = personPolicies[person].policies[count].actualAmount
            = msg.value * rate;
        personPolicies[person].count += 1;
    }
    
    // reportAccident, only by the valid accidentReporter
    function reportAccident(string flight) public {
        require(msg.sender == accidentReporter);
        
        // avoid double reporting
        if (accidents[flight].confirmed == false) {
            accidents[flight].confirmed = true;
            
            // calculate the premium sum of this flight for later claim use
            for (uint i = 0; i < flightPassengers[flight].count; i++) {
                address person = flightPassengers[flight].person[i];
    
                for (uint j = 0; j < personPolicies[person].count; j++) {
                    if (personPolicies[person].policies[j].isFlight[flight]) {
                        uint count = accidents[flight].count;
                        accidents[flight].affectedPolicies[count] = personPolicies[person].policies[j];
                        accidents[flight].count += 1;
                        accidents[flight].sumAmount += personPolicies[person].policies[j].amount;
                    }
                }
            }
            
            // cannot fully pay out
            if (accidents[flight].sumAmount > this.balance) {
                // split pro rata
                for (uint k = 0; k < accidents[flight].count; k++) {
                    accidents[flight].affectedPolicies[k].actualAmount
                        = accidents[flight].affectedPolicies[k].amount * this.balance / accidents[flight].sumAmount;
                }
            }
        }
    }
    
    // claim
    function claim(string flight) public {
        address person = msg.sender;
        
        if (accidents[flight].confirmed) {
            for (uint i = 0; i < accidents[flight].count; i++) {
                if (accidents[flight].affectedPolicies[i].person == person
                    && accidents[flight].affectedPolicies[i].paid == false) {
                    uint actualAmount = accidents[flight].affectedPolicies[i].actualAmount;
                    person.transfer(actualAmount);
                }
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
    /*function voteAccidentReporter(address newReporter) public {
        require(false);
    }*/
    
    // check balance
    function getBalance() public view returns (uint) {
        return this.balance;
    }
    
    // get one's insured trip count
    function getMyTripCount() public view returns (uint) {
        address person = msg.sender;
        return personPolicies[person].count;
    }
    
    // get one's insured trip count
    function getMyTripPolicy(uint i) public view returns (string, uint, uint, uint, bool) {
        address person = msg.sender;
        return (personPolicies[person].policies[i].flight,
            personPolicies[person].policies[i].premium,
            personPolicies[person].policies[i].amount,
            personPolicies[person].policies[i].actualAmount,
            personPolicies[person].policies[i].paid);
    }
    
}
