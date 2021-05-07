const hre = require("hardhat");
const ethers = hre.ethers;

async function main() {
	const WETH = await ethers.getContractFactory("WETH9");
	const weth = await WETH.deploy();

	await weth.deployed();

	console.log("deployed address:" + weth.address);
}

main().then(() => process.exit(0))
	.catch(error => {
		console.error(error);
		process.exit(1);
	});
