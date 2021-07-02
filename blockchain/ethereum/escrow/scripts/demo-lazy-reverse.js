// We require the Hardhat Runtime Environment explicitly here. This is optional 
// but useful for running the script in a standalone fashion through `node <script>`.
//
// When running the script with `hardhat run <script>` you'll find the Hardhat
// Runtime Environment's members available in the global scope.
const hre = require("hardhat");
const ethers = hre.ethers;

async function main() {
  // Hardhat always runs the compile task when running scripts with its command
  // line interface.
  //
  // If this script is run directly using `node` you may want to call compile 
  // manually to make sure everything is compiled
  // await hre.run('compile');

	// Assign seller and buyer
	const accounts = await ethers.getSigners();
	const seller = accounts[0];
	const buyer = accounts[1];

	// before 
	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("===========================");

	// Price of item
	const price = ethers.utils.parseEther("10.0"); // 1 eth

  // We get the contract to deploy
  const Item = await ethers.getContractFactory("LazyPurchaseReverse");
  const item1 = await Item.deploy(price);
  const item2 = await Item.deploy(price);

	// seller deploy the item sales
  await item1.deployed();
	await item2.deployed();

	console.log("seller put item1 on sale at price = " + ethers.utils.formatEther(price) + " ETH");
	console.log("seller put item2 on sale at price = " + ethers.utils.formatEther(price) + " ETH");
	console.log("...........................");

	// prepare 
	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("item1 stake:", ethers.utils.formatEther(await item1.getBalance()));
	console.log("item2 stake:", ethers.utils.formatEther(await item2.getBalance()));
	console.log("===========================");

	// buyer order item1
	const item1b = item1.connect(buyer);
	console.log("buyer order item1 on sale at price = " + ethers.utils.formatEther(price) + " ETH");
	await item1b.placeOrder({ value: price.mul(2) });

	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("item1 stake:", ethers.utils.formatEther(await item1.getBalance()));
	console.log("item2 stake:", ethers.utils.formatEther(await item2.getBalance()));
	console.log("...........................");

	// buyer order item2
	const item2b = item2.connect(buyer);
	console.log("buyer order item2 on sale at price = " + ethers.utils.formatEther(price) + " ETH");
	await item2b.placeOrder({ value: price.mul(2) });

	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("item1 stake:", ethers.utils.formatEther(await item1.getBalance()));
	console.log("item2 stake:", ethers.utils.formatEther(await item2.getBalance()));
	console.log("===========================");

	/////////////////////////////////////////////
	
	// seller acceptPurchase 1
	console.log("seller acceptPurchase item1");
	await item1.acceptPurchase({ value: price.mul(2) });

	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("item1 stake:", ethers.utils.formatEther(await item1.getBalance()));
	console.log("item2 stake:", ethers.utils.formatEther(await item2.getBalance()));
	console.log("...........................");

	// seller delivery
	console.log("seller ships the item1");
	console.log("...........................");

	// buyer confirmReceived 1
	console.log("buyer confirmReceived item1");
	await item1b.confirmReceived();

	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("item1 stake:", ethers.utils.formatEther(await item1.getBalance()));
	console.log("item2 stake:", ethers.utils.formatEther(await item2.getBalance()));
	console.log("===========================");

	///////////////////////////////////////////

	// seller acceptPurchase 2
	console.log("seller acceptPurchase item2");
	await item2.acceptPurchase({ value: price.mul(2) });

	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("item1 stake:", ethers.utils.formatEther(await item1.getBalance()));
	console.log("item2 stake:", ethers.utils.formatEther(await item2.getBalance()));
	console.log("...........................");

	// seller delivery
	console.log("seller ships the item2");
	console.log("...........................");

	// buyer confirmReceived 1
	console.log("buyer confirmReceived item2");
	await item2b.confirmReceived();

	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("item1 stake:", ethers.utils.formatEther(await item1.getBalance()));
	console.log("item2 stake:", ethers.utils.formatEther(await item2.getBalance()));
	console.log("===========================");

}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch(error => {
    console.error(error);
    process.exit(1);
  });
