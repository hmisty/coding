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

	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	
	// Price of item
	const price = ethers.utils.parseEther("10.0"); // 1 eth

	// We deploy the WETH
	const WETH = await ethers.getContractFactory("WETH9");
	const weth = await WETH.deploy();

	await weth.deployed();

	console.log("WETH deployed address: " + weth.address);
	
  // We get the contract to deploy
  const LazyPurchase = await ethers.getContractFactory("LazyPurchase");
  const item1 = await LazyPurchase.deploy(weth.address);
  const item2 = await LazyPurchase.deploy(weth.address);

  await item1.deployed();
	await item2.deployed();

  console.log("item1 deployed at:", item1.address);
  console.log("item2 deployed at:", item2.address);
	console.log("...........................");

	// seller approve WETH
	await weth.approve(item1.address, ethers.constants.MaxUint256); // approve max
	console.log("seller approve item1 for WETH");
	await weth.approve(item2.address, ethers.constants.MaxUint256); // approve max
	console.log("seller approve item2 for WETH");
	
	// seller prepare WETH
	await weth.deposit({ value: price.mul(5).div(2) });
	//await weth.deposit({ value: price.mul(2) });
	console.log("seller has WETH: " + ethers.utils.formatEther(await weth.balanceOf(seller.address)));
	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("...........................");

	// seller create the sale 1
	await item1.create(price);
	console.log("seller put item1 on sale at price = " + ethers.utils.formatEther(price) + " ETH");
	console.log("seller has WETH: " + ethers.utils.formatEther(await weth.balanceOf(seller.address)));
	console.log("ETH staked in item1:", ethers.utils.formatEther(await item1.balance()));
	console.log("WETH staked in item1:", ethers.utils.formatEther(await weth.balanceOf(item1.address)));
	console.log("...........................");

	// seller create the sale 2
	await item2.create(price);
	console.log("seller put item2 on sale at price = " + ethers.utils.formatEther(price) + " ETH");
	console.log("seller has WETH: " + ethers.utils.formatEther(await weth.balanceOf(seller.address)));
	console.log("ETH staked in item2:", ethers.utils.formatEther(await item2.balance()));
	console.log("WETH staked in item2:", ethers.utils.formatEther(await weth.balanceOf(item2.address)));
	console.log("...........................");

	/////////////////////////////////////////////

	// buyer confirmPurchase 1
	const item1b = item1.connect(buyer);
	console.log("buyer confirmPurchase item1");
	item1b.confirmPurchase({ value: price.mul(2) });
	console.log("ETH staked in item1:", ethers.utils.formatEther(await item1b.balance()));
	console.log("WETH staked in item1:", ethers.utils.formatEther(await weth.balanceOf(item1b.address)));
	console.log("seller has WETH: " + ethers.utils.formatEther(await weth.balanceOf(seller.address)));
	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("...........................");

	// seller delivery
	console.log("seller ships the goods");
	console.log("...........................");

	// buyer confirmReceived 1
	console.log("buyer confirmReceived item1");
	item1b.confirmReceived();
	console.log("ETH staked in item1:", ethers.utils.formatEther(await item1b.balance()));
	console.log("WETH staked in item1:", ethers.utils.formatEther(await weth.balanceOf(item1b.address)));
	console.log("seller has WETH: " + ethers.utils.formatEther(await weth.balanceOf(seller.address)));
	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("...........................");

	///////////////////////////////////////////

	// buyer confirmPurchase 2
	const item2b = item2.connect(buyer);
	console.log("buyer confirmPurchase item2");
	item2b.confirmPurchase({ value: price.mul(2) });
	console.log("ETH staked in item2:", ethers.utils.formatEther(await item2b.balance()));
	console.log("WETH staked in item2:", ethers.utils.formatEther(await weth.balanceOf(item2b.address)));
	console.log("seller has WETH: " + ethers.utils.formatEther(await weth.balanceOf(seller.address)));
	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("...........................");

	// seller delivery
	console.log("seller ships the goods");
	console.log("...........................");

	// buyer confirmReceived 2
	console.log("buyer confirmReceived item1");
	console.log("buyer confirmReceived item2");
	item2b.confirmReceived();
	console.log("ETH staked in item2:", ethers.utils.formatEther(await item2b.balance()));
	console.log("WETH staked in item2:", ethers.utils.formatEther(await weth.balanceOf(item2b.address)));
	console.log("seller has WETH: " + ethers.utils.formatEther(await weth.balanceOf(seller.address)));
	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
	console.log("...........................");
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch(error => {
    console.error(error);
    process.exit(1);
  });
