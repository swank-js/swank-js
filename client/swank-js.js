var SwankJS = { socket: null, connected: false, bufferedOutput: [] };

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
  this.socket = new io.Socket();
  this.socket.on(
    "connect",
    function() {
      self.connected = true;
      self.debug("connected");
      self.socket.send({ op: "handshake", userAgent: navigator.userAgent });
      if (self.bufferedOutput.length > 0) {
        for (var i = 0; i < self.bufferedOutput.length; ++i)
          self.output(self.bufferedOutput[i]);
        self.bufferedOutput = [];
      }
    });
  this.socket.on(
    "message", function swankjs_evaluate (m) {
      self.debug("eval: %o", m);
      // var m = JSON.parse(message);
      try {
        var r = window.eval(m.code);
      } catch (e) {
        self.debug("error = %s", String(e));
        self.socket.send({ op: "result", id: m.id,
                      error: String(e) + "\n" + swank_printStackTrace({ e: e }).join("\n") });
        return;
      }
      self.debug("result = %s", String(r));
      self.socket.send({ op: "result", id: m.id, error: null, values: r === undefined ? [] : [String(r)] }); });
  this.socket.on(
    "disconnect", function() {
      self.debug("connected");
    });
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
