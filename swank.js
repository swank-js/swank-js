// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t -*-
var net = require("net");
var swh = require("./swank-handler");
var swp = require("./swank-protocol");

var server = net.createServer(
  function (stream) {
    var handler = new swh.Handler(
      new swh.Executive());
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

// TBD: print connect/disconnect notifications
// TBD: handle reader errors

// function location determination:
// for code loaded from scripts: direct (if possible)
// for code entered via C-M-x etc.: use some kind of translation scheme
