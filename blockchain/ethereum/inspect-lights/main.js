const fs = require('fs');

// abi of Uni&Sushi LP token
const ABI_LP = JSON.parse(fs.readFileSync("abis/LP.abi").toString().trim());
// abi of USDT&others(ERC-20)
const ABI_USDT = JSON.parse(fs.readFileSync("abis/USDT.abi").toString().trim());

// address of main contract of lights.finance
const ADDR_MAIN = "0xeBCA2f0306C62cbE7da2808dACC147ec447936AF";
// addresses of all SLP tokens held in main contract
const ADDR_SLP = ['0xdd77c93199064a53e1db19ee0930bcdf7c9999f4',
	'0x561aef36a2237f86696e15b131f7ff3e141feab2',
  '0x06da0fd433c1a5d7a4faa01111c044910a184553',
  '0x36e2fcccc59e5747ff63a03ea2e5c0c2c14911e7',
  '0xfcfee7769c012578877dc787a5d9a339cc5920a1',
	'0x00c70e8b3c9d0e0adb85993382abaae2a11c5d96',
	'0xd11684e2ea44128c26877376cb75b9c36e8381dd'
];
// addresses of all Uni LP tokens held in main contract
const ADDR_UniLP = ['0xd3d2e2692501a5c9ca623199d38826e513033a17',
	'0x26ce49c08ee71aff0c43db8f8b9bea950b6cdc67',
	'0x3fdd067009ef5aa74c0fc3efd2b0ec88999be9f6',
	'0xa0abda1f980e03d7eadb78aed8fc1f2dd0fe83dd',
	'0x6b80eb3633901b0c5e5a4516eb8ebb9b14452b4a',
	'0xa478c2975ab1ea89e8196811f51a7b7ade33eb11',
	'0xe53bfffd5d9a53250a3f30409fdc463cb5ed05e1'
]


module.exports = async function (callback) {
	console.log("main started.");

	try {
		var accounts = await web3.eth.getAccounts();
		var account = accounts[0];
		console.log("account0: ", account);

		for (var i in ADDR_UniLP) {
			await inspect_lp(ADDR_UniLP[i]);
		}

		for (var i in ADDR_SLP) {
			await inspect_lp(ADDR_SLP[i]);
		}


	} catch (err) {
		callback(err);
	}

	callback("main ended."); //ends the program
}

async function inspect_lp(lp_addr) {
	var lp = {}, token0 = {}, token1 = {};

	lp["addr"] = lp_addr;

	lpt = new web3.eth.Contract(ABI_LP, lp_addr);
	lp["symbol"] = await lpt.methods.symbol().call();
	lp["name"] = await lpt.methods.name().call();
	lp["decimals"] = await lpt.methods.decimals().call();
	lp["totalSupply"] = await lpt.methods.totalSupply().call();
	lp["amount"] = await lpt.methods.balanceOf(ADDR_MAIN).call();
	lp["token0"] = await lpt.methods.token0().call();
	lp["token1"] = await lpt.methods.token1().call();

	var token = new web3.eth.Contract(ABI_USDT, lp["token0"]);
	token0["symbol"] = await token.methods.symbol().call();
	token0["name"] = await token.methods.name().call();
	token0["decimals"] = await token.methods.decimals().call();
	token0["amount"] = await token.methods.balanceOf(lp_addr).call();

	var token = new web3.eth.Contract(ABI_USDT, lp["token1"]);
	token1["symbol"] = await token.methods.symbol().call();
	token1["name"] = await token.methods.name().call();
	token1["decimals"] = await token.methods.decimals().call();
	token1["amount"] = await token.methods.balanceOf(lp_addr).call();

	var str = [lp["addr"], lp["symbol"], lp["name"], lp["decimals"], lp["totalSupply"]/10**lp["decimals"], lp["amount"]/10**lp["decimals"],
		lp["token0"], token0["symbol"], token0["name"], token0["decimals"], token0["amount"]/10**token0["decimals"],
		lp["token1"], token1["symbol"], token1["name"], token1["decimals"], token1["amount"]/10**token1["decimals"]].join(",");
	
	console.log(str);
}
