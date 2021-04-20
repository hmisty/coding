async function main() {
	const CSNFT = await ethers.getContractFactory("CryptoScientists");
	const csnft = await CSNFT.deploy("Hello, Crypto Scientists!");

	console.log("Crypto Scientists NFT deployed to:", csnft.address);
}

main()
	.then(() => process.exit(0))
	.catch(error => {
		console.error(error);
		process.exit(1);
	});
