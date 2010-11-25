// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t -*-
var net = require("net"), http = require('http'), io = require('socket.io');
var swh = require("./swank-handler");
var swp = require("./swank-protocol");

var swankServer = net.createServer(
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
swankServer.listen(4005, "localhost");

var httpServer = http.createServer(
  function(req, res){
    // your normal server code
    res.writeHead(200, {'Content-Type': 'text/html; charset=utf-8'});
    res.end(
        '<html><body onload="socket.connect()">' +
        '<script src="/socket.io/socket.io.js"></script>' +
	'<script>' +
	'var socket = new io.Socket();' +
	'socket.on("connect", function() { if (window.console && console.debug) console.debug("connected"); });' +
	'socket.on("message", function(message) { alert("message: " + message); });' +
	'socket.on("disconnect", function(){ alert("disconnect"); });' +
	'</script></body></html>');
  });

httpServer.listen(8009);

var socket = io.listen(httpServer);
socket.on(
  "connection", function (client) {
    // new client is here!
    console.log("client connected");
    client.on("message", function(message) {
                console.log("message from client: %s", message);
                client.send("hello! <" + message + ">");
              });
    client.on("disconnect", function() { console.log("client disconnected"); });
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
