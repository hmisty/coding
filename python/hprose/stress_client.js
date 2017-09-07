var hprose = require("hprose");
var client = hprose.Client.create("http://127.0.0.1:8181/");
var proxy = client.useService(["hello"]);

var keep_running = true;
var success = 0;
var failure = 0;
var run = () => {
  proxy.hello("world", function(result) {
    success += 1;
    //console.log(result);
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

run();
/*
   var workers = 20;
   for (var i = 0; i < workers; i++) {
   run();
   }
   */
