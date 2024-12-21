const express = require('express');
const bodyParser = require('body-parser');
const cors = require('cors');
const sha256 = require('sha256');

const app = express();
app.use(bodyParser.json());
app.use(cors());

app.post('/', (req, res) => {
  const { jsonrpc, method, params, id } = req.body;
	console.log(req.body);
  if (jsonrpc !== '2.0' || method !== 'hash' || !params || params.length !== 1) {
    res.status(400).json({ error: { code: -32600, message: 'Invalid Request' } });
    return;
  }
  const hash = sha256(params[0]);
  res.json({ jsonrpc: '2.0', result: hash, id });
});

app.listen(3000, () => console.log('JSON-RPC server listening on port 3000'));
