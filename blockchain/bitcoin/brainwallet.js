const prompt = require('prompt');
const bitcoin = require('bitcoinjs-lib');
const bip39 = require("bip39");
const bip32 = require("bip32");
const ethUtils = require('ethereumjs-util');
const createHash = require('create-hash');

const prop = [
	{
		name: 'password',
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

	if (result.password !== result.again) { return onErr('mismatched passwords. quit...'); }

	// calculate the entropy = sha256(password)
	const password = result.password;
	var entropy = createHash("sha256").update(password).digest().toString("hex");
	console.log("entropy: \033[37;47m" + entropy + "\033[0m");

	// split
	console.log("=============================================");
	
	// get priv key
	const privkey = Buffer.from(entropy, 'hex');
	const compressed = true; // default true since 2021/5/1
	const keyPair = bitcoin.ECPair.fromPrivateKey(privkey, { compressed: compressed });

	const wif_privkey = keyPair.toWIF();
	console.log("private key" + (compressed ? " (compressed)" : "") + ": \033[37;47m" + wif_privkey + "\033[0m");

	const { address } = bitcoin.payments.p2pkh({ pubkey: keyPair.publicKey });
	console.log("address: " + address);

	// split
	console.log("---------------------------------------------");

	// get mnemonic
	const mnemonic = bip39.entropyToMnemonic(entropy);
	console.log("mnemonic words (24): \033[37;47m" + mnemonic + "\033[0m"); 
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
	}
	
	// generate ethereum addresses
	const eth_derivation_metamask = "m/44'/60'/0'/0"; // m/44'60'/0'/0/x
	console.log("ETH - Metamask (" + eth_derivation_metamask + "):");

	for (var x = 0; x < 3; x++) {
		const node = master.derivePath(eth_derivation_metamask + "/" + x);
		const addr = ethUtils.toChecksumAddress(ethUtils.pubToAddress(node.publicKey, true).toString("hex"));
		console.log("eth account #0 address #" + x + ": " + addr);
	}

	const eth_derivation_ledger_live = "m/44'/60'"; // m/44'/60'/x'/0/0
	console.log("ETH - Ledger Live (" + eth_derivation_ledger_live + "):");

	for (var x = 0; x < 3; x++) {
		const node = master.derivePath(eth_derivation_ledger_live + "/" + x + "'/0/0");
		const addr = ethUtils.toChecksumAddress(ethUtils.pubToAddress(node.publicKey, true).toString("hex"));
		console.log("eth account #" + x + ": " + addr);
	}
	
});

function onErr(err) {
	console.log(err);
	return 1;
}
