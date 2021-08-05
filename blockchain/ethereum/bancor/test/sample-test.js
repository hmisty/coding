const { expect } = require("chai");

describe("Bancor", function() {
	let formula;

	beforeEach(async function() {
		const deployer = await ethers.getContractFactory("BancorFormula");
		formula = await deployer.deploy();

		await formula.deployed();
	});

	it("purchaseTargetAmount", async function() {
		const supply = 1000;
		const reserveBalance = 250;
		const reserveWeight = 500000;
		const amount = 10;
		const purchased = await formula.purchaseTargetAmount(supply, reserveBalance, reserveWeight, amount);
		expect(purchased).to.equal(19);

		const purchased2 = await formula.purchaseTargetAmount(supply, reserveBalance, reserveWeight, 1000);
		expect(purchased2).to.equal(1236);
	});

	it("fundCost", async function() {
		const supply = 1000;
		const reserveBalance = 250;
		const reserveWeight = 500000;
		const amount = 19;
		const cost = await formula.fundCost(supply, reserveBalance, reserveWeight, amount);
		expect(cost).to.equal(10);

		const cost2 = await formula.fundCost(supply, reserveBalance, reserveWeight, 1236);
		expect(cost2).to.equal(1000);
	});

});
