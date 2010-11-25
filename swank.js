// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t -*-
var net = require("net"), http = require('http'), io = require('socket.io'), util = require("util");
var swh = require("./swank-handler");
var swp = require("./swank-protocol");

var executive = new swh.Executive();

var swankServer = net.createServer(
  function (stream) {
    var handler = new swh.Handler(executive);
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
        // TBD: destroy the handler
        handler.removeAllListeners("response");
      });
  });
swankServer.listen(4005, "localhost");

function BrowserRemote (name, client) {
  this.name = name;
  this.client = client;
  this.client.on(
    "message", function(m) {
      // TBD: handle parse errors
      // TBD: validate incoming message (id, etc.)
      console.log("message from browser: %s", JSON.stringify(m));
      // var m = JSON.parse(message);
      switch(m.op) {
      case "output":
        this.output(m.str);
        break;
      case "result":
        this.sendResult(m.id, m.values);
        break;
      default:
        console.log("WARNING: cannot interpret the client message");
      }
    }.bind(this));
    this.client.on(
      "disconnect", function() {
        console.log("client disconnected: %s", this.id());
        this.disconnect();
      }.bind(this));
}

util.inherits(BrowserRemote, swh.Remote);

BrowserRemote.prototype.kind = function kind () {
  return "browser";
};

BrowserRemote.prototype.id = function id () {
  return this.name;
};

BrowserRemote.prototype.evaluate = function evaluate (id, str) {
  this.client.send(JSON.stringify({ id: id, code: str }));
};

var httpServer = http.createServer(
  function(req, res){
    // your normal server code
    res.writeHead(200, {'Content-Type': 'text/html; charset=utf-8'});
    // TBD: check message contents
    // TBD: exception handling
    // TBD: don't rely upon native json
    res.end(
        '<html><body onload="socket.connect()">' +
        '<script src="/socket.io/socket.io.js"></script>' +
	'<script>' +
	'var socket = new io.Socket();' +
	'socket.on("connect", function() { if (window.console && console.debug) console.debug("connected"); });' +
	'socket.on("message", function(message) { if (window.console && console.debug) console.debug("eval: %o", message); var m = JSON.parse(message); var r = window.eval(m.code); if (window.console && console.debug) console.debug("r = %o", r); socket.send({ op: "result", id: m.id, values: r === undefined ? [] : [String(r)] }); });' +
	'socket.on("disconnect", function(){ if (window.console && console.debug) console.debug("connected"); });' +
	'</script></body></html>');
  });

httpServer.listen(8009);

var socket = io.listen(httpServer);
socket.on(
  "connection", function (client) {
    // new client is here!
    console.log("client connected");
    executive.attachRemote(new BrowserRemote("browser", client));
  });

// TBD: print connect/disconnect notifications
// TBD: client-side logging facility
// TBD: handle reader errors

// function location determination:
// for code loaded from scripts: direct (if possible)
// for 'compiled' code: load the code by adding <script> tag loaded from the swank-js' webserver, its name should encode the real path and line offset
// for code entered via REPL: none
// PREPROCESS STACK TRACES!!!
// https://github.com/emwendelin/Javascript-Stacktrace
// ALSO: http://blog.yoursway.com/2009/07/3-painful-ways-to-obtain-stack-trace-in.html -- onerror in ie gives the innermost frame
// it should be also possible to 'soft-trace' functions so that they extend Exception objects with caller info as it passes through them
// TBD: unix domain sockets, normal slime startup

/*
(slime-repl-shortcut-eval-async '(js:list-remotes) 'message)
(slime-repl-shortcut-eval-async '(js:select-remote 2 nil) 'message)
 */