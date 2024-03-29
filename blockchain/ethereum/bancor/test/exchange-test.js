const { expect } = require("chai");

describe("Exchange", function() {
	let erc20;
	let exchange;
	let owner;

	const name = "A3 Token";
	const symbol = "AAA";
	const totalSupply = ethers.utils.parseEther('10000.0');

	beforeEach(async function() {
		const deployer1 = await ethers.getContractFactory("ERC20");
		erc20 = await deployer1.deploy(name, symbol, totalSupply);
		await erc20.deployed();

		const deployer = await ethers.getContractFactory("TokenExchange");
		exchange = await deployer.deploy(erc20.address);
		await exchange.deployed();

		// put all ERC20 into the exchange
		erc20.transfer(exchange.address, totalSupply);
		// save the owner
		owner = await exchange.owner();
	});

	it("ERC20", async function() {
		expect(await erc20.name()).to.equal(name);
		expect(await erc20.symbol()).to.equal(symbol);
		expect(await erc20.totalSupply()).to.equal(totalSupply);

		// check balance
		expect(await erc20.balanceOf(exchange.address)).to.equal(totalSupply);
		expect(await erc20.balanceOf(owner)).to.equal(0);
	});

	it("TokenExchange", async function() {
		expect(await exchange.token()).to.equal(erc20.address);
		expect(await exchange.initialized()).to.equal(false);
	});

	it("purchase", async function() {
		const supply = ethers.utils.parseEther('1000.0');
		const reserveBalance = ethers.utils.parseEther('250.0');
		const reserveWeight = 500000;
		const amount = ethers.utils.parseEther('10.0');

		// initialize the exchange
		await exchange.initialize(supply, reserveBalance, reserveWeight);
		expect(await exchange.initialized()).to.equal(true);

		// purchase
		const options = {
			value: amount
		}
		await exchange.purchase(options);
		const n = (await erc20.balanceOf(owner)).toString();
		expect(n).to.equal("19803902718556966005"); // 19.8039
	});

});
