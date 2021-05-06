require("@nomiclabs/hardhat-waffle");
require("@nomiclabs/hardhat-web3");
require("@nomiclabs/hardhat-etherscan");

//.secrets format: { "privkey":"....", "alchemyapikey":"...." }
const { privkey, alchemy_apikey, etherscan_apikey } = require("./.secrets.json");

// This is a sample Hardhat task. To learn how to create your own go to
// https://hardhat.org/guides/create-task.html
task("accounts", "Prints the list of accounts", async () => {
  const accounts = await ethers.getSigners();

  for (const account of accounts) {
    console.log(account.address);
  }
});

// Added by Evan
task("balance", "Prints an account's balance [Evan's]")
	.addParam("account", "The account's address")
	.setAction(async taskArgs => {
		const account = web3.utils.toChecksumAddress(taskArgs.account);
		const balance = await web3.eth.getBalance(account);

		console.log(web3.utils.fromWei(balance, "ether"), "ETH");
	});

// You need to export an object to set up your config
// Go to https://hardhat.org/config/ to learn more

/**
 * @type import('hardhat/config').HardhatUserConfig
 */
module.exports = {
  //solidity: "0.7.3",
  solidity: "0.8.0",
	networks: {
		rinkeby: {
			url: `https://eth-rinkeby.alchemyapi.io/v2/${alchemy_apikey}`,
			accounts: [`0x${privkey}`]
		}
	},
	etherscan: {
		apiKey: etherscan_apikey
	}

};

