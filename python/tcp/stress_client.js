var net = require('net');

var host = '127.0.0.1';
var port = 8181;

var client = new net.Socket();
client.connect(port, host, () => {
	console.log('connected to ' + host + ':' + port);
});

var keep_running = true;
var success = 0;
var failure = 0;

client.on('data', (data) => {
	success += 1;

	if (keep_running) {
		client.write('0'); 
	} else {
		client.write('exit');
		client.destroy();
	}

});

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
	client.write('0');
}
