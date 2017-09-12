var bson = require('bson');
var net = require('net');

var Proxy = function(){
	this.connection = null;
};

Proxy.prototype.connect = function() {
};

Proxy.prototype.use_service = function(names) {
	var conn = this.connection;
	for (var name in names) {
		Proxy.prototype[name] = function() {
			var args = Array.prototype.splice.call(arguments, 1, arguments.length);
			var doc = {
				fn: name,
				args: args,
			};
			var rpc_data = obj.serialize(doc);
			conn.write(rpc_data); //FIXME if conn is not connected
		};
	}
};

Proxy.prototype.on_result = function(callback) {
	var conn = this.connection;
	conn.on('data', (data) => {
		var obj = new bson();
		var doc = obj.deserialize(data);
		if (doc.error_code == 0) {
			callback(doc.result);
		} else {
			error(doc.error_code, doc.error_msg);
		}
	});
};

Proxy.prototype.disconnect = function() {
};

/*
var bson_rpc = require('bson_rpc');

var host = '127.0.0.1';
var port = 8181;

var client = bson_rpc.create(host, port);

var server = client.use_service(['hi', 'echo', 'add']);
proxy.hi((data) => {
	console.log('received: ' + JSON.stringify(data));
});*/

var obj = new bson();
var doc = {
	fn: 'add',
	args: [1, 2],
};
var rpc_data = obj.serialize(doc);
console.log('data: ', rpc_data);

var doc2 = obj.deserialize(rpc_data);
console.log('doc2: ', doc2);

var host = '127.0.0.1';
var port = 8181;
var client = new net.Socket();

var keep_running = true;
var success = 0;
var failure = 0;

client.on('data', (data) => {
	var obj = new bson();
	var doc = obj.deserialize(data);
	//console.log('received doc: ', doc);
	if (doc.error_code == 0 && doc.result == 3) { //1+2=3
		success += 1;
	} else {
		failure += 1;
		console.log('error! ', doc);
	}
	
	if (keep_running) {
		client.write(rpc_data);
	} else {
		client.destroy();
	}
});

client.connect(port, host, () => {
	console.log('connected');

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

	var workers = 1; //20;
	for (var i = 0; i < workers; i++) {
		client.write(rpc_data);
	}

});

