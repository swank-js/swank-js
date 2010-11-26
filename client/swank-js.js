var SwankJS = {};

// TBD: check message contents
// TBD: exception handling
// TBD: don't rely upon native json

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
    "message", function(message) {
      self.debug("eval: %o", message);
      var m = JSON.parse(message);
      var r = window.eval(m.code);
      self.debug("result = %s", String(r));
      socket.send({ op: "result", id: m.id, values: r === undefined ? [] : [String(r)] }); });
  socket.on(
    "disconnect", function() {
      self.debug("connected");
    });
  socket.connect();
};
