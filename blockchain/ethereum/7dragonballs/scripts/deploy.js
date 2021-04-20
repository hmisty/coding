async function main() {
	const [deployer] = await ethers.getSigners();

	console.log("Deploying contracts with the account:", deployer.address);
	console.log("Account balance:", (await deployer.getBalance()).toString());

	const NFT = await ethers.getContractFactory("SevenDragonBalls");
	const nft = await NFT.deploy();

	console.log("7 Dragon Balls NFT deployed to:", nft.address);
}

main()
	.then(() => process.exit(0))
	.catch(error => {
		console.error(error);
		process.exit(1);
	});
