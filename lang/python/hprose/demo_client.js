var hprose = require("hprose");
var client = hprose.Client.create("http://127.0.0.1:8181/");
var proxy = client.useService(["hello"]);
proxy.hello("world", function(result) {
      console.log(result);
});
