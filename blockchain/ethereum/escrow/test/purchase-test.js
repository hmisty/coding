const { expect } = require("chai");

describe("Purchase", function() {
	var buyer, seller;
	var purchase, purchase4buyer;

	before(async function() {
		accounts = await ethers.getSigners();
		seller = accounts[0];
		buyer = accounts[1];

    const Purchase = await ethers.getContractFactory("Purchase");
    purchase = await Purchase.deploy({ value: 200 });
    
    await purchase.deployed();

		purchase4buyer = purchase.connect(buyer);
	});

  it("after deploy, escrowed balance should be 200", async function() {
    expect(await purchase.balance()).to.equal(200);
  });

	it("value of item to purchase should be 100", async function() {
		expect(await purchase.value()).to.equal(100);
	});


	it("after confirmPurchase, escrowed balance should be 400", async function() {
		await purchase4buyer.confirmPurchase({ value: 200 });
		expect(await purchase.balance()).to.equal(400);
	});

	it("after confirmReceived, escrowed balance should be 0", async function() {
		await purchase4buyer.confirmReceived();
		expect(await purchase.balance()).to.equal(0);
	});

});
