const chai = require("chai");
const assert = chai.assert;
const bs58check = require('bs58check');
const bip39 = require("bip39");
const bip32 = require("bip32");
const createHash = require('create-hash');

var ethUtils = require('ethereumjs-util');

function addr(node) {
	var hash = node.identifier; // hash160 of the publicKey

	const payload = Buffer.allocUnsafe(21);

	var version = 0x0;
	payload.writeUInt8(version, 0);

	hash.copy(payload, 1);

	var address = bs58check.encode(payload);
	return address;
}

//--------------------------------------------------

/*
https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki

bip32: entropy (N bits) <=> mnemonic words =[HMAC-SHA512]=> master seed (128 bits entropy) => master node (m) => wallet accounts (m/0, m/1, ...) => chains (m/0/0 - internal, m/0/1 - external, ...) => addresses (m/0/0/0, m/0/0/1, ..., m/0/0/k; m/0/1/0, m/0/1/1, ..., m/0/1/k; ...)

real use: puzzle =[SHA-256]=> entropy (N bits) <=> mnemonic words =[HMAC-SHA512]=> master seed (128 bits entropy) => master node (m) => purpose (44') => coin (

128 bits entropy => 128 /32 * 3 = 12 words
256 bits entroyp => 256 /32 * 3 = 24 words

derive path: https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki

sha256(puzzle) => 24 words => addresses... for bitcoins (m/44'/0'/0'/0/0,1,2, ...) and ethereum (m/44'/60'/0'/0/0,1,2,3...)

test cases: 

puzzle => treasury island map & nobody knows

entropy = sha256(puzzle) => a8fd89fcc8496d1536b57d91ba60eda4069dbb4ec62bea889b2ca510c6419f14

mnemonic words => pottery unable left mountain nothing melody suspect quick museum spread derive elite hawk tank deposit shell tunnel dynamic sleep enforce arrest camp labor lucky

m/44'/0'/0'/0/0	=> 15BSxBZd6H1wnc8zFCnPSwEFctUi3PT5R9
m/44'/0'/0'/0/1	=> 12RezqqjZiWGM7hLJUQtoYE2LL8ZcwXV9w
m/44'/0'/0'/0/2	=> 1DKVJExWPPa7riHQ3SKLcGmDxSMzNDmiuG

verified by https://iancoleman.io/bip39/

m/44'/60'/0'/0/0 => 0xf2f77563e9C335846A9021794371Ebcb5C517ec5
m/44'/60'/0'/0/1 => 0x56080204a432Ed97265d96dFddc8E3D0FaBbd272
m/44'/60'/0'/0/2 => 0x7a8BbD4fe43182fD031C0Db3FBdcCde9905d9DF6

verified by https://iancoleman.io/bip39/ and metamask
*/

var puzzle = "treasury island map & nobody knows";
//$ echo "treasury island map & nobody knows" | shasum -a 256
//1943970f6a4a59ea3875fe6582d8ed324dd77d8a68da0eb99e463e947112daea  - //WRONG! echo will append an extra \n
//$ perl -e 'print qq(treasury island m6
//a8fd89fcc8496d1536b57d91ba60eda4069dbb4ec62bea889b2ca510c6419f14  -

var entropy = createHash("sha256").update(puzzle).digest().toString("hex");
console.log("entropy: " + entropy); //a8fd89fcc8496d1536b57d91ba60eda4069dbb4ec62bea889b2ca510c6419f14

var mnemonic = bip39.entropyToMnemonic(entropy);
console.log("mnemonic: " + mnemonic); //pottery unable left mountain nothing melody suspect quick museum spread derive elite hawk tank deposit shell tunnel dynamic sleep enforce arrest camp labor lucky

/*
var mnemonic = "test test test test test test test test test test test junk"; // cold

var entropy = bip39.mnemonicToEntropy(mnemonic);
console.log("entropy: " + entropy);
*/


//--------------------------------------------------

// testcases #1
var btc_addresses = [
	["m/44'/0'/0'/0/0", "15BSxBZd6H1wnc8zFCnPSwEFctUi3PT5R9"],
	["m/44'/0'/0'/0/1", "12RezqqjZiWGM7hLJUQtoYE2LL8ZcwXV9w"],
	["m/44'/0'/0'/0/2", "1DKVJExWPPa7riHQ3SKLcGmDxSMzNDmiuG"]
];

// testcases #2
var eth_addresses = [
	["m/44'/60'/0'/0/0", "0xf2f77563e9C335846A9021794371Ebcb5C517ec5"],
	["m/44'/60'/0'/0/1", "0x56080204a432Ed97265d96dFddc8E3D0FaBbd272"],
	["m/44'/60'/0'/0/2", "0x7a8BbD4fe43182fD031C0Db3FBdcCde9905d9DF6"]
];

//--------------------------------------------------

var passphrase = "";

var seed = bip39.mnemonicToSeedSync(mnemonic, passphrase);
var master = bip32.fromSeed(seed); // m

// method 1 + test cases #1

for (var i = 0; i < btc_addresses.length; i++) {
	var node = master.derivePath(btc_addresses[i][0]);
	var address = addr(node);
	try {
		assert.equal(address, btc_addresses[i][1]);
		console.log("passed " + i + "/" + btc_addresses.length);
		//console.log(node.toWIF()); // show private key. verified with https://iancoleman.io/bip39/ and Electrum
	} catch (e) {
		console.log(e);
	}
}

// generate 10 btc addresses...
for (var i = 0; i < 10; i++) {
	var node = master.derivePath("m/44'/0'/0'/0/" + i);
	var address = addr(node);
	console.log("btc #" + i + ": " + address);
}

// method 2 + test cases #2

var purpose = master.deriveHardened(44); // m/44', 44 means BIP-44 HD Wallet
//var coin_type = purpose.deriveHardened(0); // m/44'/0', 0 means BTC
var coin_type = purpose.deriveHardened(60); // m/44'/60', 60 means ETH
var account = coin_type.deriveHardened(0); // m/44'/x'/0', 0 means Wallet(account) 0

for (var i = 0; i < eth_addresses.length; i++) {
	var path_type = account.derive(0); // m/44'/x'/0'/0, 0 means external(main), 1 means internal(change)
	var node = path_type.derive(i); // m/44'/x'/0'/0/0, 0 means the first address
	var address = ethUtils.pubToAddress(node.publicKey, true).toString("hex"); // to sanitize
	var checksum_address = ethUtils.toChecksumAddress(address);
	//console.log(checksum_address);
	try {
		assert.equal(checksum_address, eth_addresses[i][1]);
		console.log("passed " + i + "/" + eth_addresses.length);
		//console.log("0x" + node.privateKey.toString("hex"));
	} catch (e) {
		console.log(e);
	}
}

// generate 10 eth addresses...
for (var i = 0; i < 10; i++) {
	var node = master.derivePath("m/44'/60'/0'/0/" + i);
	var address = ethUtils.toChecksumAddress(ethUtils.pubToAddress(node.publicKey, true).toString("hex"));
	console.log("eth #" + i + ": " + address);
}

