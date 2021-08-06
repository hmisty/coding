const { expect } = require("chai");

describe("Exchange", function() {
	let erc20;
	let exchange;

	const name = "A3 Token";
	const symbol = "AAA";
	const totalSupply = 10000000000000;

	beforeEach(async function() {
		const deployer1 = await ethers.getContractFactory("ERC20");
		erc20 = await deployer1.deploy(name, symbol, totalSupply);
		await erc20.deployed();

		const deployer = await ethers.getContractFactory("TokenExchange");
		exchange = await deployer.deploy(erc20.address);
		await exchange.deployed();

		// put all ERC20 into the exchange
		erc20.transfer(exchange.address, totalSupply);
	});

	it("ERC20", async function() {
		expect(await erc20.name()).to.equal(name);
		expect(await erc20.symbol()).to.equal(symbol);
		expect(await erc20.totalSupply()).to.equal(totalSupply);
		expect(await erc20.balanceOf(exchange.address)).to.equal(totalSupply);
	});

	it("TokenExchange", async function() {
		expect(await exchange.token()).to.equal(erc20.address);
		expect(await exchange.initialized()).to.equal(false);
	});

	it("purchase", async function() {
		const supply = 1000;
		const reserveBalance = 250;
		const reserveWeight = 500000;
		const amount = 10;

		// initialize the exchange
		await exchange.initialize(supply, reserveBalance, reserveWeight);
		expect(await exchange.initialized()).to.equal(true);

		// purchase
		
	});

});
