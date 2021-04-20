const { expect } = require("chai");

describe("Crypto Scientists", function() {
	var csnft;

	before(async function() {
		const CSNFT = await ethers.getContractFactory("CryptoScientists");
		csnft = await CSNFT.deploy();

		await csnft.deployed();
	});

	describe("A few common tests", function() {
		it("Should return the nft count", async function() {
			expect(await csnft.token_count()).to.equal(0);
		});

		it("Should return the nft name", async function() {
			expect(await csnft.name()).to.equal("Crypto Scientists NFT");
		});

		it("Should return the nft symbol", async function() {
			expect(await csnft.symbol()).to.equal("CSNFT");
		});
	});

});
