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

  // We get the contract to deploy
  const Purchase = await ethers.getContractFactory("Purchase");
  const purchase = await Purchase.deploy({ value: price.mul(2) });

  await purchase.deployed();

  console.log("item on board at:", purchase.address);
	console.log("escrowed balance:", ethers.utils.formatEther(await purchase.balance()));
	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));

	// confirmPurchase
	const bp = purchase.connect(buyer);
	console.log("buyer confirmPurchase()");
	bp.confirmPurchase({ value: price.mul(2) });
	console.log("escrowed balance:", ethers.utils.formatEther(await purchase.balance()));
	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));

	// delivery
	console.log("seller ships the goods");

	// confirmReceived
	console.log("buyer confirmReceived()");
	bp.confirmReceived();
	console.log("escrowed balance:", ethers.utils.formatEther(await purchase.balance()));
	console.log("seller balance:", ethers.utils.formatEther(await seller.getBalance()));
	console.log("buyer balance:", ethers.utils.formatEther(await buyer.getBalance()));
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch(error => {
    console.error(error);
    process.exit(1);
  });
