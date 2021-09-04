const prompt = require('prompt');
const bitcoin = require('bitcoinjs-lib');
const bip39 = require("bip39");
const bip32 = require("bip32");
const base58check = require('base58check');
const ethUtils = require('ethereumjs-util');
const createHash = require('create-hash');
const createKeccakHash = require('keccak');
const BN = require('bn.js');
const EC = require('elliptic').ec;

//----------------------------------------------

function onErr(err) {
	console.log(err);
	return 1;
}

function _genPubKeyKeccak(curve, privkey, compressed) {
	const ec = new EC(curve); // secp256k1, or p256 for secp256r1
	const ecparams = ec.curve;

  const pubKey = Buffer.from(ec.keyFromPrivate(privkey).getPublic(compressed, true)).slice(1);
	//TODO assert(pubKey.length == 64);

	var hash = createKeccakHash('keccak256');
	hash.update(pubKey);
	const addr = hash.digest().slice(-20).toString('hex');
	const checksumAddr = ethUtils.toChecksumAddress(addr);

	return checksumAddr;
}

function genAddrETH(privkey, compressed) {
	return _genPubKeyKeccak('secp256k1', privkey, compressed);
};

function genAddrNEW(privkey, compressed) {
	const NewChainDevNetId = 1002;
	const NewChainTestNetId = 1007;
	const NewChainMainNetId = 1012;
	const PREFIX = 'NEW';

	const hexAddress2NewAddress = function (hexAddress, chainId) {
		if (hexAddress === undefined) {
			return ''
		}
		hexAddress = hexAddress.trim()
		if (typeof (hexAddress) === 'string' && hexAddress.startsWith(PREFIX)) {
			return hexAddress
		}
		if (hexAddress.startsWith('0x')) {
			hexAddress = hexAddress.slice(2)
		}
		if (hexAddress.length !== 40) {
			return null
		}
		chainId = Number(chainId)
		let data = chainId.toString(16).slice(-8) + hexAddress
		if (data.length % 2 !== 0) {
			data = '0' + data
		}
		return PREFIX + base58check.encode(data)
	}

	const addrETH = _genPubKeyKeccak('p256', privkey, compressed);
	const addrNEW = hexAddress2NewAddress(addrETH, NewChainMainNetId);
	return addrNEW;
}

//----------------------------------------------
const tests = {
	"entropy": "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08",
	"privkey": "5K2YUVmWfxbmvsNxCsfvArXdGXm7d5DC9pn4yD75k2UaSYgkXTh",
	"addrBTC": "1HKqKTMpBTZZ8H5zcqYEWYBaaWELrDEXeE",
	"addrETH": "0x2a260a110bc7b03f19C40a0BD04FF2C5DCB57594",
	"addrNEW": "NEW182Z4JpaaNQpD5ejJHEKatFRzNpd1XWwBMJ2",
	"mnemonic": "panel custom call awesome sick ready hamster wool patch client reduce clip desk pole hole gesture lion grief firm subway force job choice bargain",
	"ledgerBTC0": "1FT2xCHANeDPNdG1QtZbuRytpZYn9AJSZg",
	"ledgerBTC1": "1M88LeBTcoiKQ2x6QfztdzaHPsiZWinpif",
	"ledgerBTC2": "1w5t6PBfXd2Y3ofkZ4dnVjZhyoQucy3BL",
	"metamaskETH0": "0xc399E4e21ECE8E2a34150A17d248d0C8a77C1d06",
	"metamaskETH1": "0x110efe73740AB7F42BC16652497B85b533C5ae7C",
	"metamaskETH2": "0xf86f3D9fCF9C836FAAa9829dfe5FA16D059346e9",
	"ledgerETH0": "0xc399E4e21ECE8E2a34150A17d248d0C8a77C1d06",
	"ledgerETH1": "0xA50846e3CCF8b72ab304B3E169425F9678F9524b",
	"ledgerETH2": "0xB2909C993D9eBaB2a3F6333d4254D8f93cf2d96b"
};

const checklog = (function () {
	var success = 0, fail = 0;
	const N = Object.keys(tests).length;

	return function (key, value) {
		if (tests[key] == value) {
			console.log("\u2705 " + key + " is correct");
			success += 1;
		} else {
			console.log("\u274C " + key + " is incorrect");
			fail += 1;
		}

		if (success + fail == N) {
			console.log("\u{1F590} ...all tests are finished: " + success + " succeeded, " + fail + " failed.");
		}
	};

})();

const prop = [
	/*{
		name: 'compressed',
		description: 'compressed (true/false)?',
		type: 'boolean',
		default: false
	},*/
	{
		name: 'password',
		description: 'password (type "test" for running tests)',
		hidden: true,
		replace: '*'
	},
	{
		name: 'again',
		hidden: true,
		replace: '*'
	}
];

prompt.start();

prompt.get(prop, function (err, result) {
	if (err) { return onErr(err); }

	// empty again hints recovery intention instead of generating
	if (result.again != "" && result.password !== result.again) {
		return onErr('mismatched passwords. quit...'); 
	}

	// calculate the entropy = sha256(password)
	const password = result.password;

	// if password == 'test', run test cases
	var testing = password == 'test';
	if (testing) console.log("\u2705 running tests...");

	// split
	console.log("=============================================");

	// calculate entropy from password
	var entropy = createHash("sha256").update(password).digest().toString("hex");
	console.log("entropy: \033[37;47m" + entropy + "\033[0m");
	if (testing) checklog('entropy', entropy);

	// split
	console.log("=============================================");
	
	// get priv key
	const privkey = Buffer.from(entropy, 'hex'); // privkey is Buffer(entropy)

	const compressed = result.compressed || false; // use uncompressed by default
	const keyPair = bitcoin.ECPair.fromPrivateKey(privkey, { compressed: compressed });

	const wif_privkey = keyPair.toWIF();
	console.log("BTC private key" + (compressed ? " (compressed)" : "") + ": \033[37;47m" + wif_privkey + "\033[0m");
	if (testing) checklog('privkey', wif_privkey);

	const { address } = bitcoin.payments.p2pkh({ pubkey: keyPair.publicKey });
	console.log("BTC address: " + address);
	if (testing) checklog('addrBTC', address);

	// ethereum only uses uncompressed ec
	if (compressed == false) {
		// split
		console.log("---------------------------------------------");

		// gen Ethereum address, use compressed = false for compatibility
		const eth_address = genAddrETH(privkey, compressed);
		console.log("ETH private key: the same as entropy");
		console.log("ETH address: " + eth_address);
		if (testing) checklog('addrETH', eth_address);

		// gen NEW address, use compressed = false for compatibility
		const new_address = genAddrNEW(privkey, compressed);
		console.log("NEW private key: the same as entropy");
		console.log("NEW address: " + new_address);
		if (testing) checklog('addrNEW', new_address);
	}

	// split
	console.log("---------------------------------------------");

	//!!! below are unware of compressed or uncompressed above
	
	// get mnemonic
	const mnemonic = bip39.entropyToMnemonic(entropy);
	console.log("mnemonic words (24): \033[37;47m" + mnemonic + "\033[0m"); 
	if (testing) checklog('mnemonic', mnemonic);

	const words24 = mnemonic.split(" ");
	const copy = [];
	words24.forEach((w, i) => { copy.push((i+1) + ":" + w) });
	console.log("indexed: \033[37;47m" + copy.join(" ") + "\033[0m");

	const passphrase = ""; // use no passphrase
	const seed = bip39.mnemonicToSeedSync(mnemonic, passphrase);
	const master = bip32.fromSeed(seed); // m

	// generate bitcoin addresses
	// following ledger live standard
	const btc_derivation_ledger_live = "m/44'/0'"; // m/44'/0'/x'/0/0
	console.log("BTC - Ledger Live (" + btc_derivation_ledger_live + "):");
	
	for (var x = 0; x < 3; x++) {
		const node = master.derivePath(btc_derivation_ledger_live + "/" + x + "'/0/0");
		const { address } = bitcoin.payments.p2pkh({ pubkey: node.publicKey });
		console.log("btc account #" + x + ": " + address);
		if (testing) checklog('ledgerBTC'+x, address);
	}
	
	// generate ethereum addresses
	const eth_derivation_metamask = "m/44'/60'/0'/0"; // m/44'60'/0'/0/x
	console.log("ETH - Metamask (" + eth_derivation_metamask + "):");

	//-------- hd wallet ---------
	for (var x = 0; x < 3; x++) {
		const node = master.derivePath(eth_derivation_metamask + "/" + x);
		const addr = ethUtils.toChecksumAddress(ethUtils.pubToAddress(node.publicKey, true).toString("hex"));
		console.log("eth account #0 address #" + x + ": " + addr);
		if (testing) checklog('metamaskETH'+x, addr);
	}

	const eth_derivation_ledger_live = "m/44'/60'"; // m/44'/60'/x'/0/0
	console.log("ETH - Ledger Live (" + eth_derivation_ledger_live + "):");

	for (var x = 0; x < 3; x++) {
		const node = master.derivePath(eth_derivation_ledger_live + "/" + x + "'/0/0");
		const addr = ethUtils.toChecksumAddress(ethUtils.pubToAddress(node.publicKey, true).toString("hex"));
		console.log("eth account #" + x + ": " + addr);
		if (testing) checklog('ledgerETH'+x, addr);
	}
	
});
