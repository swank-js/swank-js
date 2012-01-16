
/*
 * Add this script to your html file to connect to swank-js
 */


// TODO don't hardcode swank server
var swank_server = "http://localhost:8009/";

function load(url, requirement) {
  if (requirement) {
    var fulfilled = requirement.call(window);
    if (!fulfilled) {
      window.setTimeout(function() { load(url, requirement); }, 100);
      return;
    }
  }
  var script = document.createElement('script');
  script.setAttribute('type', 'text/javascript');
  script.setAttribute('src', swank_server + url);
  document.head.appendChild(script);
}

function loadTests() {
  var query = document.location.toString().split("?")[1];
  return query && query.match(/swank-js-tests/);
}

load("swank-js/json2.js");
load("socket.io/socket.io.js");
load("swank-js/stacktrace.js");
load("swank-js/swank-js.js", function() { return !!window.io; });
load("swank-js/swank-completion.js", function() { return !!window.SwankJS; });

if (loadTests()) {
  load("swank-js/browser-tests.js", function() { return window.SwankJS && SwankJS.doCompletion; });
}