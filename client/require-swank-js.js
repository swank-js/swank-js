(function () {
  // TBD: URL should be placed here by the server
  var URL = "http://localhost:8009/";
  window.swank_server = URL;
  var shim = {};
  function u (path) { return URL + path + ".js"; }
  function p (path, v) {
    v.deps = v.deps || [];
    for (var i = 0; i < v.deps.length; ++i)
      v.deps[i] = u(v.deps[i]);
    shim[u(path)] = v;
  }

  p("swank-js/json2", { exports: "JSON" });
  p("swank-js/stacktrace", { exports: "swank_printStackTrace" });
  p("swank-js/swank-js", {
    exports: "SwankJS",
    deps: [ "swank-js/json2", "socket.io/socket.io", "swank-js/stacktrace" ]
  });
  p("swank-js/completion", {
    exports: "Completion"
  });
  p("swank-js/load", {
    deps: ["swank-js/swank-js", "swank-js/completion"]
  });

  // FIXME: should update modules to be RequireJS-compatible
  requirejs.config({ shim: shim });
  define([u("swank-js/load")], function () {});
})();
