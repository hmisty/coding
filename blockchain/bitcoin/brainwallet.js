// npm install -g prompt bitcoinjs-lib ecpair tiny-secp256k1 bip39 bip32 base58check ethereumjs-util create-hash keccak bn.js elliptic
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

const ECPairFactory = require('ecpair').default;
const ecc = require('tiny-secp256k1');

const ECPair = ECPairFactory(ecc);
const BIP32 = bip32.BIP32Factory(ecc);

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
	const addr = '0x' + hash.digest().slice(-20).toString('hex');
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

function printHdWallet(seed, label, testSuffix, testing) {
	const master = BIP32.fromSeed(seed);
	console.log("m/purpose'/coin_type'/account'/change/address_index [" + label + "]");

	// BTC - Native SegWit
	const btc_derivation_native = "m/84'/0'/0'/0";
	console.log("\nBTC - Native SegWit (" + btc_derivation_native + ") [" + label + "]:");
	for (var x = 0; x < 3; x++) {
		const nodeNative = master.derivePath(`${btc_derivation_native}/${x}`);
		const { address: nativeAddr } = bitcoin.payments.p2wpkh({ pubkey: Buffer.from(nodeNative.publicKey) });
		console.log("native segwit #" + x + ": " + nativeAddr);
		if (testing) checklog('bip39segwit'+x+testSuffix, nativeAddr);
	}

	// BTC - Ledger Live
	const btc_derivation_ledger_live = "m/44'/0'";
	console.log("\nBTC - Ledger Live (" + btc_derivation_ledger_live + ") [" + label + "]:");
	for (var x = 0; x < 3; x++) {
		const node = master.derivePath(btc_derivation_ledger_live + "/" + x + "'/0/0");
		const { address } = bitcoin.payments.p2pkh({ pubkey: Buffer.from(node.publicKey) });
		console.log("btc account #" + x + ": " + address);
		if (testing) checklog('ledgerBTC'+x+testSuffix, address);
	}

	// ETH - Metamask
	const eth_derivation_metamask = "m/44'/60'/0'/0";
	console.log("\nETH - Metamask (" + eth_derivation_metamask + ") [" + label + "]:");
	for (var x = 0; x < 3; x++) {
		const node = master.derivePath(eth_derivation_metamask + "/" + x);
		const addr = ethUtils.toChecksumAddress('0x' + ethUtils.pubToAddress(Buffer.from(node.publicKey), true).toString("hex"));
		console.log("eth account #0 address #" + x + ": " + addr);
		if (testing) checklog('metamaskETH'+x+testSuffix, addr);
	}

	// ETH - Ledger Live
	const eth_derivation_ledger_live = "m/44'/60'";
	console.log("\nETH - Ledger Live (" + eth_derivation_ledger_live + ") [" + label + "]:");
	for (var x = 0; x < 3; x++) {
		const node = master.derivePath(eth_derivation_ledger_live + "/" + x + "'/0/0");
		const addr = ethUtils.toChecksumAddress('0x' + ethUtils.pubToAddress(Buffer.from(node.publicKey), true).toString("hex"));
		console.log("eth account #" + x + ": " + addr);
		if (testing) checklog('ledgerETH'+x+testSuffix, addr);
	}
}

//----------------------------------------------
const tests = {
	"entropy": "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08",
	"privkey": "5K2YUVmWfxbmvsNxCsfvArXdGXm7d5DC9pn4yD75k2UaSYgkXTh",
	"addrBTC": "1HKqKTMpBTZZ8H5zcqYEWYBaaWELrDEXeE",
	"privkeyC": "L2ZovMyTxxQVJmMtfQemgVcB5YmiEDapDwsvX6RqvuWibgUNRiHz",
	"segwit": "bc1qtmrl9526rusw4dnavrcfal72tz6ram5lqzutru",
	"addrETH": "0x2a260a110bc7b03f19C40a0BD04FF2C5DCB57594",
	"addrNEW": "NEW182Z4JpaaNQpD5ejJHEKatFRzNpd1XWwBMJ2",
	"mnemonic": "panel custom call awesome sick ready hamster wool patch client reduce clip desk pole hole gesture lion grief firm subway force job choice bargain",
	"mnemonic12": "panel custom call awesome sick ready hamster wool patch client reduce clay",
	"ledgerBTC0": "1FT2xCHANeDPNdG1QtZbuRytpZYn9AJSZg",
	"ledgerBTC0_12": "17Pdu784JVMZCJvSf11FdszjCXJtLGgqxf",
	"ledgerBTC1": "1M88LeBTcoiKQ2x6QfztdzaHPsiZWinpif",
	"ledgerBTC1_12": "1Lf843ZUgyuqZ4XqAqjthgZ3BNrb3p2oqY",
	"ledgerBTC2": "1w5t6PBfXd2Y3ofkZ4dnVjZhyoQucy3BL",
	"ledgerBTC2_12": "1HW8G8CTh1m4nWb4fn5iwp1MievCGKZYez",
	"bip39segwit0": "bc1qquzeycp8sfh6jwdm9pq92m6zp8vgv3p0l5fuyz",
	"bip39segwit0_12": "bc1qlhycuafdegfkcdx435t2glqkyrrrrqfpdjd2nq",
	"bip39segwit1": "bc1qv5u53h298fh3htgyxe32fwuvt9huz8e4rjjqnh",
	"bip39segwit1_12": "bc1qn6y5jfdf5069w9mxcwjp3qeazqrvr8vltfkv4n",
	"bip39segwit2": "bc1qcfk9m6m87068jcqtxhqrsssp478x9grjjcssqs",
	"bip39segwit2_12": "bc1q9urqudztnhffup66l8hs4zzcm3a3e2v63w06fe",
	"metamaskETH0": "0xc399E4e21ECE8E2a34150A17d248d0C8a77C1d06",
	"metamaskETH0_12": "0xEfc03835916c8403e29B0EC0Cf2c1DE248cde7bf",
	"metamaskETH1": "0x110efe73740AB7F42BC16652497B85b533C5ae7C",
	"metamaskETH1_12": "0x555bf7f0193969D0f9Eb16D086064ade4cbB3f35",
	"metamaskETH2": "0xf86f3D9fCF9C836FAAa9829dfe5FA16D059346e9",
	"metamaskETH2_12": "0x5707962250E998ab76620742925f8235BB48e0fB",
	"ledgerETH0": "0xc399E4e21ECE8E2a34150A17d248d0C8a77C1d06",
	"ledgerETH0_12": "0xEfc03835916c8403e29B0EC0Cf2c1DE248cde7bf",
	"ledgerETH1": "0xA50846e3CCF8b72ab304B3E169425F9678F9524b",
	"ledgerETH1_12": "0xF6FE4347f2313f8Ae70B072D1DaAF0172993E2e9",
	"ledgerETH2": "0xB2909C993D9eBaB2a3F6333d4254D8f93cf2d96b",
	"ledgerETH2_12": "0x553225CD475947abfd6037ED5C110A7E8e348A81",

	"entropy_9999": "068ab2b4a069c2ccba8267fce87433c9e259a509009b781b661c7f3e6932c521",
	"privkey_9999": "5HsAgYyyKzhH45Axx4P7umAkcRXM2DuyKJGaP87CjzZJjLZUZxo",
	"addrBTC_9999": "1Mig3mSpDzbkfZ91UJsTjDnF7QJtxWtmoN",
	"privkeyC_9999": "KwSRjaATcqemrZQyx1vdVsx3VMEwNPzoZmDkLeNNpm3bFmeUD3Dr",
	"segwit_9999": "bc1qvktvghw9veesuucxaqmsh2u20na7gedhh9f86s",
	"addrETH_9999": "0x99048aA36B69C0f8e2EE64Db17a568d142B55017",
	"addrNEW_9999": "NEW182NaFoqMeP47UpoX5BdTfzEztsUVrReVCAC",
	"mnemonic_9999": "alley few pudding dolphin order green tube erupt woman dry artefact need cereal spoon catch beach job swamp mandate wrist track offer behind disease",
	"mnemonic12_9999": "alley few pudding dolphin order green tube erupt woman dry artefact nasty",
	"bip39segwit0_9999": "bc1qh67chfkpnmtp5qvs9zh9xm5meff7h4s5xcg4gn",
	"bip39segwit1_9999": "bc1q4evqkr6sul6d30qeru4sswvu98lmcaxa9d2qy9",
	"bip39segwit2_9999": "bc1qmv9f2mjkxf7469kgzh0qdkpl3q74k8trathl3l",
	"ledgerBTC0_9999": "1DvEPiAkV4eRrM4VfvfexEMeqmPGFBkAN2",
	"ledgerBTC1_9999": "1N4G5a7PzhJSpkuroHotPtKXUcnqmJ5TT1",
	"ledgerBTC2_9999": "1Gx5BkC4DytBm7cofefDn1n236cP19yHPf",
	"metamaskETH0_9999": "0xd7e6640059bD31ED2bba3f1dAB95348E4A7e3Bfd",
	"metamaskETH1_9999": "0xd5fFd8103f1D59d75422EdA8E0999a97465313b3",
	"metamaskETH2_9999": "0x470dC3812D9B7933C3895BAB8bA21CB2D2eDCE18",
	"ledgerETH0_9999": "0xd7e6640059bD31ED2bba3f1dAB95348E4A7e3Bfd",
	"ledgerETH1_9999": "0x76c1bFAA2A92f64b2C5b7518243E219fdF4299E5",
	"ledgerETH2_9999": "0xBa00289Bd76609028B43669648F5D08Af2C848BB",

	"bip39segwit0_9999_12": "bc1qudnacy6s0dp5zxy4qfe38dn58kdnk8pcfw5pyx",
	"bip39segwit1_9999_12": "bc1qrsfn9rv7l073x76ftv2plc4nh8ahqxch3325fa",
	"bip39segwit2_9999_12": "bc1q49ghnhnuq2999a0vj778rnsqjqrpj7l3zwfqtv",
	"ledgerBTC0_9999_12": "1C42C2g12JuVBhb25rzvCxHfWgXeM3AkiL",
	"ledgerBTC1_9999_12": "1MBHA3mUwyfPCNXXiHec6JafuKQDriJFkX",
	"ledgerBTC2_9999_12": "1HPszFMZ41u3HYoMFqBqpP9yytg5ZXHhwp",
	"metamaskETH0_9999_12": "0xCe7c3f8802726c1451807A24f3fCA88a7043B6f7",
	"metamaskETH1_9999_12": "0x8582F37b8bEAe573F73060d860f60EbC0Cea7dB3",
	"metamaskETH2_9999_12": "0x0434bA42F57c4E4280E8E5ef2F35e220E2786FC4",
	"ledgerETH0_9999_12": "0xCe7c3f8802726c1451807A24f3fCA88a7043B6f7",
	"ledgerETH1_9999_12": "0xcF9F2225dB15B1D7CB694ec7a92A204c72C7D38c",
	"ledgerETH2_9999_12": "0xBeB9Cf1816f68cA887fc16Ec7F6eE4e3C3a4a9ce"
};

const checklog = (function () {
	var success = 0, fail = 0, called = 0;

	return function (key, value) {
		if (!(key in tests)) return;
		called++;
		if (tests[key] == value) {
			console.log("\u2705 " + key + " is correct");
			success += 1;
		} else {
			console.log("\u274C " + key + " is incorrect (expected: " + tests[key] + ")");
			fail += 1;
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
		name: 'iterations',
		description: 'SHA256 iterations (default: 1)',
		type: 'integer',
		default: 1
	},
	{
		name: 'password',
		description: 'password (type "test" for running tests)',
		//hidden: false,
		hidden: true,
		replace: '*'
	},
	{
		name: 'again',
		//hidden: false,
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

	// calculate entropy from password with optional iterations
	var iterations = result.iterations || 1;
	var suffix = (testing && iterations != 1) ? '_' + iterations : '';
	var testEq = function (key, value) { if (testing) checklog(key + suffix, value); };
	var hash = createHash("sha256").update(password).digest();
	for (var i = 1; i < iterations; i++) {
		hash = createHash("sha256").update(hash).digest();
	}
	var entropy = hash.toString("hex");
	console.log("SHA256 x" + iterations + " entropy: \033[37;47m" + entropy + "\033[0m");
	testEq('entropy', entropy);

	// split
	console.log("=============================================");
	
	// get priv key
	const privkey = Buffer.from(entropy, 'hex'); // privkey is Buffer(entropy)

	const compressed = result.compressed || false; // use uncompressed by default
	//const keyPair = bitcoin.ECPair.fromPrivateKey(privkey, { compressed: compressed }); //bitcoinjs-lib < 5.x.x
	const keyPair = ECPair.fromPrivateKey(privkey, { compressed: compressed });

	const wif_privkey = keyPair.toWIF();
	console.log("BTC private key" + (compressed ? " (compressed)" : "") + ": \033[37;47m" + wif_privkey + "\033[0m");
	testEq('privkey', wif_privkey);

	//const { address } = bitcoin.payments.p2pkh({ pubkey: keyPair.publicKey });
	const { address } = bitcoin.payments.p2pkh({ pubkey: Buffer.from(keyPair.publicKey) });
	console.log("BTC address: " + address + (compressed ? " (compressed)":" (uncompressed)"));
	testEq('addrBTC', address);

	// segwit can use only compressed privkey
	//const keyPairC = bitcoin.ECPair.fromPrivateKey(privkey, { compressed: true}); //bitcoinjs-lib < 5.x.x
	const keyPairC = ECPair.fromPrivateKey(privkey, { compressed: true});
	const wif_privkeyC = keyPairC.toWIF();
	console.log("segwit private key (compressed)" + ": \033[37;47m" + wif_privkeyC + "\033[0m");
	testEq('privkeyC', wif_privkeyC);

	//const { address: segwit_address } = bitcoin.payments.p2wpkh({ pubkey: keyPairC.publicKey });
	const { address: segwit_address } = bitcoin.payments.p2wpkh({ pubkey: Buffer.from(keyPairC.publicKey) });
	console.log("segwit address: " + segwit_address);
	testEq('segwit', segwit_address);

	// ethereum only uses uncompressed ec
	if (compressed == false) {
		// split
		console.log("---------------------------------------------");

		// gen Ethereum address, use compressed = false for compatibility
		const eth_address = genAddrETH(privkey, compressed);
		console.log("ETH private key: the same as entropy");
		console.log("ETH address: " + eth_address);
		testEq('addrETH', eth_address);

		// gen NEW address, use compressed = false for compatibility
		const new_address = genAddrNEW(privkey, compressed);
		console.log("NEW private key: the same as entropy");
		console.log("NEW address: " + new_address);
		testEq('addrNEW', new_address);
	}

	// split
	console.log("---------------------------------------------");

	//!!! below are unware of compressed or uncompressed above

	const passphrase = "";
	
	//---------------------------------------------
	// 24-word mnemonic
	//---------------------------------------------
	const mnemonic24 = bip39.entropyToMnemonic(entropy);
	console.log("mnemonic words (24): \033[37;47m" + mnemonic24 + "\033[0m"); 
	testEq('mnemonic', mnemonic24);

	const words24 = mnemonic24.split(" ");
	const copy24 = [];
	words24.forEach((w, i) => { copy24.push((i+1) + ":" + w) });
	console.log("indexed: \033[37;47m" + copy24.join(" ") + "\033[0m");

	const seed24 = bip39.mnemonicToSeedSync(mnemonic24, passphrase);
	printHdWallet(seed24, "24 words", suffix, testing);

	//---------------------------------------------
	// 12-word mnemonic
	//---------------------------------------------
	console.log("---------------------------------------------");
	const entropy12 = entropy.slice(0, 32);
	const mnemonic12 = bip39.entropyToMnemonic(entropy12);
	console.log("mnemonic words (12): \033[37;47m" + mnemonic12 + "\033[0m");
	testEq('mnemonic12', mnemonic12);

	const words12 = mnemonic12.split(" ");
	const copy12 = [];
	words12.forEach((w, i) => { copy12.push((i+1) + ":" + w) });
	console.log("indexed: \033[37;47m" + copy12.join(" ") + "\033[0m");

	const seed12 = bip39.mnemonicToSeedSync(mnemonic12, passphrase);
	printHdWallet(seed12, "12 words", suffix + '_12', testing);

});
