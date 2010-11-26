var SwankJS = { socket: null };

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

SwankJS.output = function output (str) {
  if (this.socket)
    this.socket.send({ op: "output", str: str });
};

SwankJS.setup = function setup () {
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
      self.debug("connected");
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

SwankJS.loadScripts = function loadScripts () {
  var scripts = document.getElementsByTagName("script");
  var parent = document.getElementsByTagName("head")[0] || document.body;
  for (var i = 0; i < scripts.length; ++i) {
    if (scripts[i].src && /\/swank-js\//.test(scripts[i].src)) {
      this.base = scripts[i].src.replace(/\/swank-js\/.*$/, "");
      // document.write(
      //   '<script type="text/javascript" src="' + this.base + '/socket.io/socket.io.js"></script>' +
      //     '<script type="text/javascript" src="' + this.base + '/swank-js/stacktrace.js"></script>' +
      //     '<script type="text/javascript">SwankJS.setup();</script>');
      parent.appendChild(this.makeScriptElement(this.base + "/socket.io/socket.io.js"));
      parent.appendChild(this.makeScriptElement(this.base + "/swank-js/stacktrace.js"));
      parent.appendChild(this.makeScriptElement(this.base + "/swank-js/load.js"));
      break;
    }
  }
  if (i == scripts.length)
    SwankJS.debug("WARNING: cannot locate /swank-js/ script tag");
};

SwankJS.loadScripts();
