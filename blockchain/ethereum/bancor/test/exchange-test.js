const { expect } = require("chai");

describe("Exchange", function() {
	let erc20;
	let exchange;

	beforeEach(async function() {
		const deployer1 = await ethers.getContractFactory("ERC20");
		erc20 = await deployer1.deploy("A3 Token", "AAA", 10000000000000);
		await erc20.deployed();

		const deployer = await ethers.getContractFactory("TokenExchange");
		exchange = await deployer.deploy(erc20.address);
		await exchange.deployed();
	});

	it("ERC20", async function() {
		expect(await erc20.name()).to.equal("A3 Token");
		expect(await erc20.symbol()).to.equal("AAA");
	});

	it("TokenExchange", async function() {
		expect(await exchange.token()).to.equal(erc20.address);
		expect(await exchange.initialized()).to.equal(false);
	});

});
