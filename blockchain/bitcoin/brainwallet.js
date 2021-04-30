const prompt = require('prompt');
const bitcoin = require('bitcoinjs-lib');
const wif = require('wif');
const bs58check = require('bs58check');
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
	
	// get priv key
	const privkey = Buffer.from(entropy, 'hex');
	const keyPair = bitcoin.ECPair.fromPrivateKey(privkey, { compressed: false });

	const wif_privkey = keyPair.toWIF();
	console.log("private key: \033[37;47m" + wif_privkey + "\033[0m");

	const { address } = bitcoin.payments.p2pkh({ pubkey: keyPair.publicKey });
	console.log("address: " + address);


});

function onErr(err) {
	console.log(err);
	return 1;
}
