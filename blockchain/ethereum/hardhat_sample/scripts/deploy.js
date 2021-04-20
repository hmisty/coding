async function main() {
	const [deployer] = await ethers.getSigners();

	console.log("Deploying contracts with the account:", deployer.address);
	console.log("Account balance:", (await deployer.getBalance()).toString());

	const CSNFT = await ethers.getContractFactory("CryptoScientists");
	const csnft = await CSNFT.deploy();

	console.log("Crypto Scientists NFT deployed to:", csnft.address);
}

main()
	.then(() => process.exit(0))
	.catch(error => {
		console.error(error);
		process.exit(1);
	});
