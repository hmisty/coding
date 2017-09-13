'use strict';

var bson = require('bson');
var net = require('net');

var Proxy = function(host, port){
	this.host = host;
	this.port = port;
	this.connection = new net.Socket();
};

Proxy.prototype.connect = function(callback) {
	this.connection.connect(this.port, this.host, callback);
};

Proxy.prototype.use_service = function(names) {
	var conn = this.connection;
	for (var i in names) {
		var name = names[i];
		Proxy.prototype[name] = function() {
			var args = Array.prototype.splice.call(arguments, 0, arguments.length);
			var doc = {
				fn: name,
				args: args,
			};
			var obj = new bson();
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
			callback(null, doc.result);
		} else {
			var err = JSON.stringify(doc);
			callback(err);
		}
	});
};

Proxy.prototype.disconnect = function() {
	this.connection.destroy();
};


module.exports = Proxy;
