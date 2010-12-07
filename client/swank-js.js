//
// Copyright (c) 2010 Ivan Shvedunov. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//
// * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following
// disclaimer in the documentation and/or other materials
// provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

var SwankJS = {
  socket: null,
  connected: false,
  bufferedOutput: [],
  pingId: 1,
  lastMessageTime: 0,
  pingIntervalId: null,
  reconnectIntervalId: null,
  pingEnabled: true,
  evaluating: false
};

SwankJS.CONNECTION_TIMEOUT = 3000;
SwankJS.RECONNECT_ATTEMPT_INTERVAL = 2000;
SwankJS.PING_INTERVAL = 900;

// TBD: check message contents
// TBD: exception handling
// TBD: trim stack trace excluding everything starting from swankjs_evaluate line

SwankJS.debug = function debug () {
  if (!window.console)
    return;
  var debug = console.debug || console.log;
  if (!debug)
    return;
  var args = [];
  for (var i = 0; i < arguments.length; ++i)
    args.push(arguments[i]);
  debug.apply(console, args);
};

SwankJS.setPingEnabled = function setPingEnabled (enable) {
  enable = !!enable;
  if (this.pingEnabled == enable)
    return;
  this.pingEnabled = enable;
  if (this.pingEnabled)
    this.startPing();
  else
    this.stopPing();
};

SwankJS.setup = function setup () {
  try {
    if (parent.window && parent.window.document !== document && parent.window.SwankJS)
      return;
  } catch (e) {}
  var self = this;
  // TBD: swank-js should proxy all requests to autoadd its scripts
  // (this way, the dynamic script loading stuff isn't necessary)
  // and to make flashsocket swf load from the same url as the
  // web app itself.
  // Don't forget about 'Host: ' header though!
  this.lastMessageTime = new Date().getTime();
  this.socket = new io.Socket();
  this.socket.on(
    "connect",
    function() {
      if (self.reconnectIntervalId !== null) {
        clearInterval(self.reconnectIntervalId);
        self.reconnectIntervalId = null;
      }
      self.connected = true;
      self.debug("connected");
      self.socket.send({ op: "handshake", userAgent: navigator.userAgent });
      if (self.bufferedOutput.length > 0) {
        for (var i = 0; i < self.bufferedOutput.length; ++i)
          self.output(self.bufferedOutput[i]);
        self.bufferedOutput = [];
      }
      self.lastMessageTime = new Date().getTime();
      self.startPing();
    });
  this.socket.on(
    "message", function swankjs_evaluate (m) {
      self.lastMessageTime = new Date().getTime();
      if (m.hasOwnProperty("pong"))
        return;

      self.debug("eval: %o", m);
      // JS reentrancy is possible. I've observed it when using XSLTProcessor
      self.evaluating = true;
      try {
        var r = window.eval(m.code);
      } catch (e) {
        var message = String(e);
        if (message == "[object Error]") {
          try {
            message = "ERROR: " + e.message;
          } catch(e1) {}
        }
        self.debug("error = %s", message);
        self.socket.send({ op: "result", id: m.id,
                           error: message + "\n" + swank_printStackTrace({ e: e }).join("\n") });
        return;
      } finally {
        // don't let the connection break due to missed pings when evaluation takes too long
        self.lastMessageTime = new Date().getTime();
        self.evaluating = false;
      }
      self.debug("result = %s", String(r));
      self.socket.send({ op: "result", id: m.id, error: null, values: r === undefined ? [] : [String(r)] }); });
  this.socket.on(
    "disconnect", function() {
      self.debug("disconnected");
      self.connected = false;
    });
  this.socket.connect();
};

SwankJS.startPing = function startPing () {
  var self = this;
  if (this.pingIntervalId === null && this.pingEnabled && this.connected)
    this.pingIntervalId = setInterval(
      function () {
        self.ping();
      }, this.PING_INTERVAL);
};

SwankJS.stopPing = function stopPing () {
  if (this.pingIntervalId !== null) {
    clearInterval(this.pingIntervalId);
    this.pingIntervalId = null;
  }
};

SwankJS.ping = function ping () {
  if (!this.pingEnabled) {
    this.stopPing();
    return;
  }

  if (this.evaluating)
    return;

  var self = this;
  if (new Date().getTime() > this.lastMessageTime + this.CONNECTION_TIMEOUT) {
    this.debug("ping timeout");
    if (this.pingIntervalId !== null) {
      this.stopPing();
      this.reconnectIntervalId = setInterval(
        function () {
          self.reconnect();
        }, this.RECONNECT_ATTEMPT_INTERVAL);
    }
  } else if (this.connected)
    this.socket.send({ op: "ping", id: this.pingId++ });
};

SwankJS.reconnect = function reconnect () {
  this.debug("reconnecting");
  this.socket.disconnect();
  this.socket.connect();
};

// useful functions for the REPL / web apps

SwankJS.output = function output (str) {
  if (this.socket && this.connected)
    this.socket.send({ op: "output", str: str });
  else
    this.bufferedOutput.push(str);
};

SwankJS.reload = function reload () {
  document.location.reload(true);
};

SwankJS.refreshCSS = function refreshCSS () {
  // FIXME: this doesn't work in IE yet
  // FIXME: support refresh of individual CSS files
  var links = document.getElementsByTagName('link');
  for (var i = 0; i < links.length; i++) {
    var link = links[i];
    if (link.rel.toLowerCase().indexOf('stylesheet') >=0 && link.href) {
      var h = link.href.replace(/(&|\\?)forceReload=\d+/, "");
      link.href = h + (h.indexOf('?') >= 0 ? '&' : '?') + 'forceReload=' + Date.now();
    }
  }
};

/*
// we may need this later

SwankJS.makeScriptElement = function makeScriptElement (src, content) {
  var script = document.createElement("script");
  script.type = "text/javascript";
  if (src)
    script.src = src;
  else {
    var text = document.createTextNode(content);
    script.appendChild(text);
  }
  return script;
};
*/

SwankJS.setup();
