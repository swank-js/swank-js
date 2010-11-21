// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t -*-
var net = require("net");
var swh = require("./swank-handler");
var swp = require("./swank-protocol");
var Script = process.binding('evals').Script;
var evalcx = Script.runInContext;

var server = net.createServer(
  function (stream) {
    var context = Script.createContext();
    for (var i in global) context[i] = global[i];
    context.module = module;
    context.require = require;
    var handler = new swh.Handler(
      new swh.Executive(
        { evaluate: function (str) { return evalcx(str, context, "repl"); } }));
    var parser = new swp.SwankParser(
      function onMessage (message) {
        handler.receive(message);
      });
    handler.on(
      "response", function (response) {
        var responseText = swp.buildMessage(response);
        console.log("response: %s", responseText);
        stream.write(responseText);
      });
    stream.on(
      "data", function (data) {
        parser.execute(data);
      });
    stream.on(
      "end", function () {
        // FIXME: notify handler -> executive
      });
  });
server.listen(4005, "localhost");

// TBD: handle reader errors
