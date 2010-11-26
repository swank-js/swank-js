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
