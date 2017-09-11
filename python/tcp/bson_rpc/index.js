var bson = require('bson');
var net = require('net');

var Proxy = function () {};
Proxy.prototype = {
	host: '',
	port: 0,
	client: null,
	create: function (host, port) {
		this.host = host;
		this.port = port;

		/*
			 client.connect(port, host, () => {
			 console.log('connected to ' + host + ':' + str(port));
			 client.on('data', (data) => {
			 var obj = new bson();
			 var doc = obj.deserialize(data);
			 callback_receive(doc);
			 });

			 callback_send();
			 */
	},
		use_service: function(names) {
			for (var name in names) {
				Proxy.prototype[name] = function() {
					var doc = {
						fn: name,
						args: Array.prototype.splice.call(arguments, 1, arguments.length)
					};

					console.log('call remote fn: ' + name + ' with doc: ' + JSON.stringify(doc));

					var obj = new bson();
					var data = obj.serialize(doc);
					client.write(data);
				}
			}
		},
		disconnect: function() {
			client.destroy();
			console.log('disconnected from ' + host + ':' + str(port));
		},
	};

	module.exports = Proxy;
