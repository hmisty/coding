var request = require('request');

var payload = {
  "method": "echo",
  "params": ["echome!"],
  "jsonrpc": "2.0",
  "id": 0,
};

var options = {
  url: 'http://localhost:4000/jsonrpc',
  method: "POST",
  json: true,
  headers: {"content-type": "application/json"},
  body: payload,
};

request(options, (error, response, body) => {
  if (!error && response.statusCode == 200) {
    console.log(body);
  } else {
    console.log(error);
  }
});
