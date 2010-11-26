// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t -*-
var net = require("net"), http = require('http'), io = require('socket.io'), util = require("util"),
    url = require('url'), fs = require('fs');
var swh = require("./swank-handler");
var swp = require("./swank-protocol");

var executive = new swh.Executive();

var swankServer = net.createServer(
  function (stream) {
    stream.setEncoding("utf-8");
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
      switch(m.op) {
      case "output":
        this.output(m.str);
        break;
      case "result":
        if (m.error) {
          this.output(m.error + "\n");
          this.sendResult(m.id, []);
          break;
        }
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
  this.client.send({ id: id, code: str });
};

// proxy code from http://www.catonmat.net/http-proxy-in-nodejs
function proxyRequest (request, response) {
  var proxy = http.createClient(8080, "localhost"); // TBD: use configurable host //request.headers['host']);
  var proxyRequest = proxy.request(request.method, request.url, request.headers);
  proxyRequest.addListener(
    'response', function (proxyResponse) {
      proxyResponse.addListener(
        'data', function(chunk) {
          response.write(chunk, 'binary');
        });
      proxyResponse.addListener(
        'end', function() {
          response.end();
        });
      response.writeHead(proxyResponse.statusCode, proxyResponse.headers);
    });
  request.addListener('data', function(chunk) {
    proxyRequest.write(chunk, 'binary');
  });
  request.addListener('end', function() {
    proxyRequest.end();
  });
};

var httpServer = http.createServer(
  function serveClient(req, res) {
    var path = url.parse(req.url).pathname, parts, cn;
    // console.log("%s %s", req.method, req.url);
    if (path && path.indexOf("/swank-js/") != 0) {
      // console.log("--> proxy");
      proxyRequest(req, res);
      return;
    }
    var file = path.substr(1).split('/').slice(1);
    var clientVersion = "0.1";
    var clientFiles = {
      'stacktrace.js': 'stacktrace.js',
      'swank-js.js': 'swank-js.js',
      'load.js': 'load.js',
      'test.html': 'test.html'
    };
    var types = {
      html: "text/html; charset=utf-8",
      js: "text/javascript; charset=utf-8"
    };

    function write (path) {
      if (req.headers['if-none-match'] == clientVersion) {
        res.writeHead(304);
        res.end();
      } else {
        res.writeHead(200, clientFiles[path].headers);
        res.end(clientFiles[path].content, clientFiles[path].encoding);
      }
    };

    var localPath = clientFiles[file];

    if (req.method == 'GET' && localPath !== undefined){
      // TBD: reenable caching, check datetime of the file
      // if (path in clientFiles){
      //   write(path);
      //   return;
      // }

      fs.readFile(
        __dirname + '/client/' + localPath, function(err, data){
          if (err) {
            res.writeHead(404, {'Content-Type': 'text/plain; charset=utf-8'});
            res.end("file not found");
          } else {
            var ext = localPath.split('.').pop();
            clientFiles[localPath] = {
              headers: {
                'Content-Length': data.length,
                'Content-Type': types[ext],
                'ETag': clientVersion
              },
              content: data,
              encoding: ext == 'swf' ? 'binary' : 'utf8'
            };
            write(localPath);
          }
        });
    } else {
      res.writeHead(404, {'Content-Type': 'text/plain; charset=utf-8'});
      res.end("file not found");
    }
  });

httpServer.listen(8009);

var socket = io.listen(httpServer);
socket.on(
  "connection", function (client) {
    // new client is here!
    console.log("client connected");
    executive.attachRemote(new BrowserRemote("browser", client));
  });

// TBD: proper UTF-8 handling
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
// TBD: http request logging (for specific remote)
// TBD: pointing the proxy to different server
