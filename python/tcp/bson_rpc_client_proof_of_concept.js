var bson = require('bson');
var net = require('net');

var Proxy = function () {};
Proxy.prototype = {
	host: '',
	port: 0,
	client: null,
	fn: '',
	connect: function (host, port) {
	}
	remote: function() {
		var doc = {
			fn: this.fn,
			args: Array.prototype.splice.call(arguments, 0, arguments.length)
		};
	}
};

var server = new Proxy();
server.remote.call({fn:"add"}, 1, 2);

/*
var obj = new bson();
var doc = {
	fn: 'add',
	args: [1, 2],
};
var data = obj.serialize(doc);
console.log('data: ', data);

var doc2 = obj.deserialize(data);
console.log('doc2: ', doc2);

var host = '127.0.0.1';
var port = 8181;
var client = new net.Socket();
client.connect(port, host, () => {
	console.log('connected');
	for (var i = 0; i < 1000; i++) {
		client.write(data);
	}
	client.destroy();
});

client.on('data', (data) => {
	console.log('data: ', data);
	var obj = new bson();
	var doc = obj.deserialize(data);
	console.log('=> doc: ', doc);

	//client.destroy();
});

client.on('close', () => {
	console.log('conn closed');
});
*/
