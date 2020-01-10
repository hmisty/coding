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

var keep_running = true;
var success = 0;
var failure = 0;

var run = () => {
  request(options, (error, response, body) => {
    if (!error && response.statusCode == 200) {
      success += 1;
      //console.log(body);
    } else {
      failure += 1;
      //console.log(error);
    }
    if (keep_running)
      run();
  });
}

var time_to_run = 5 * 1000; // 5 seconds
var start = Date.now();
setTimeout(() => {
  keep_running = false;
  
  var elapsed = Date.now() - start;
  console.log('Time elapsed: ' + elapsed + ' ms');
  console.log('Successful requests: ' + success);
  console.log('Failed requests: ' + failure);
  console.log('Request per second: ' + Math.floor(success / elapsed * 1000));
}, time_to_run); //stop after predefined seconds

var workers = 20;
for (var i = 0; i < workers; i++) {
  run();
}
