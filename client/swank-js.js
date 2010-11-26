var SwankJS = {};

// TBD: check message contents
// TBD: exception handling
// TBD: don't rely upon native json
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
  var self = this;
  var socket = new io.Socket();
  socket.on(
    "connect",
    function() {
      self.debug("connected");
    });
  socket.on(
    "message", function swankjs_evaluate (message) {
      self.debug("eval: %o", message);
      var m = JSON.parse(message);
      try {
        var r = window.eval(m.code);
      } catch (e) {
        self.debug("error = %s", String(e));
        socket.send({ op: "result", id: m.id,
                      error: String(e) + "\n" + swank_printStackTrace({ e: e }).join("\n") });
        return;
      }
      self.debug("result = %s", String(r));
      socket.send({ op: "result", id: m.id, error: null, values: r === undefined ? [] : [String(r)] }); });
  socket.on(
    "disconnect", function() {
      self.debug("connected");
    });
  socket.connect();
};
