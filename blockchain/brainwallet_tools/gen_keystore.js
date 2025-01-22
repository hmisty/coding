/* Evan Liu, deepseek
 * 2025.1.22
 *
 * prerequsite:
 * npm install ethereumjs-wallet crypto
 */
const Wallet = require('ethereumjs-wallet'); // 注意导入方式
const fs = require('fs');
const crypto = require('crypto');
const prompt = require('prompt');

// 配置 prompt 的样式
prompt.message = '';
prompt.delimiter = '';

// 定义输入 schema
const schemaPasswords = {
	properties: {
		keystorePassword1: {
			description: '请输入 Keystore 密码:',
			required: true,
			hidden: true,
			replace: '*', // 输入时显示星号
		},
		keystorePassword2: {
			description: '请再次输入 Keystore 密码:',
			required: true,
			hidden: true,
			replace: '*', // 输入时显示星号
		},
	},
};

const schemaPassphrase = {
	properties: {
		passphrase: {
			description: '请输入脑钱包口令:',
			required: true,
			hidden: true,
			replace: '*', // 输入时显示星号
		},
	},
};

// 启动 prompt
prompt.start();

// 获取用户输入
prompt.get(schemaPasswords, async (err, result) => {
	if (err) {
		console.error('输入错误:', err);
		return;
	}

	const { keystorePassword1, keystorePassword2 } = result;

	// 校验两次输入的 Keystore 密码是否一致
	if (keystorePassword1 !== keystorePassword2) {
		console.error('错误: 两次输入的 Keystore 密码不一致！');
		return;
	}

	// 如果密码一致，继续提示用户输入脑钱包口令
	prompt.get(schemaPassphrase, async (err, result) => {
		if (err) {
			console.error('输入错误:', err);
			return;
		}

		const { passphrase } = result;

		// 计算脑钱包口令的 SHA-256 作为私钥
		const privateKey = crypto.createHash('sha256').update(passphrase).digest('hex');

		// 从私钥创建钱包
		const wallet = Wallet['default'].fromPrivateKey(Buffer.from(privateKey, 'hex'));

		// 获取生成的地址
		const address = wallet.getAddressString();
		console.log(`生成的地址: ${address}`);

		// 生成 Keystore
		const keystore = await wallet.toV3(keystorePassword1, { kdf: 'scrypt', n: 262144 });
		//console.log(keystore);

		// 生成文件名
		const now = new Date();
		const date = now.toISOString().split('T')[0].replace(/-/g, '');
		const timestamp = now.getTime();
		const filename = `keystore-${date}-${timestamp}.json`;

		// 保存 Keystore 文件
		fs.writeFileSync(filename, JSON.stringify(keystore));
		console.log(`Keystore 文件已生成: ${filename}`);
	});
});
