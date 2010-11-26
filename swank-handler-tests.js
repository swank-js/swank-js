// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t -*-
var swh = require("./swank-handler");
var readFromString = require("./lisp").readFromString;
var util = require("util");
var assert = require("assert");

var expected = [];
var executive = new swh.Executive({ pid: 4242 });
var handler = new swh.Handler(executive);

handler.on(
  "response", function (response) {
    assert.ok(expected.length > 0);
    assert.ok(typeof(response) == "string");
    // console.log("response: %s", response);
    var expectedResponse = expected.shift();
    if (expectedResponse instanceof RegExp)
      assert.ok(expectedResponse.test(response));
    else
      assert.equal(expectedResponse, response);
  });

function expect () {
  for (var i = 0; i < arguments.length; ++i)
    expected.push(arguments[i]);
}

function verifyExpectations () {
  // console.log("expected: %s\n", expected.map(JSON.stringify).join("\n"));
  assert.equal(0, expected.length);
}

function request (str) {
  for (var i = 1; i < arguments.length; ++i)
    expected.push(arguments[i]);
  handler.receive(readFromString(str));
  verifyExpectations();
}

request('(:emacs-rex (swank:connection-info) "COMMON-LISP-USER" t 1)',
        '(:return (:ok (:encoding (:coding-system "utf-8" :external-format "UTF-8") ' +
                       ':lisp-implementation (:name "JS" :type "JS" :version "1.5") ' +
                       ':package (:name "JS" :prompt "JS") ' +
                       ':pid 4242)) ' +
        '1)');

// currently we just ignore swank-require
request('(:emacs-rex (swank:swank-require \'(swank-listener-hooks swank-indentation)) "COMMON-LISP-USER" t 2)',
        '(:return (:ok nil) 2)');

request('(:emacs-rex (swank:create-repl nil) "COMMON-LISP-USER" t 3)',
        '(:return (:ok ("JS" "JS")) 3)');

request('(:emacs-rex (swank:listener-eval "3 * 10\n") "JS" :repl-thread 4)',
        '(:return (:ok (:values "30")) 4)');

request('(:emacs-rex (swank:listener-eval "undefined") "JS" :repl-thread 5)',
        '(:return (:ok nil) 5)');

request('(:emacs-rex (swank:autodoc \'("zzzz" swank::%cursor-marker%) :print-right-margin 236)' +
        ' "COMMON-LISP-USER" :repl-thread 6)',
        '(:return (:ok :not-available) 6)');

request('(:emacs-rex (swank:listener-eval "_swank.output(\'hello world\\\\n\')") "JS" :repl-thread 7)',
        '(:write-string "hello world\n")',
        '(:return (:ok nil) 7)');

request('(:emacs-rex (swank:listener-eval "_swank.output(1234)") "JS" :repl-thread 8)',
        '(:write-string "1234")',
        '(:return (:ok nil) 8)');

request('(:emacs-rex (swank:listener-eval "zzz") "JS" :repl-thread 9)',
        /^\(:write-string "ReferenceError: zzz is not defined(.|\n)*"\)$/,
        '(:return (:ok nil) 9)');

// TBD: debugger

function FakeRemote (name) {
  this.name = name;
};

util.inherits(FakeRemote, swh.Remote);

FakeRemote.prototype.kind = function kind () {
  return "test";
};

FakeRemote.prototype.id = function id () {
  return this.name;
};

FakeRemote.prototype.evaluate = function evaluate (id, str) {
  this.sendResult(id, [ "R:" + this.name + ":" + str.replace(/^\s*|\s*$/g, "") ]);
};

request('(:emacs-rex (js:list-remotes) "JS" :repl-thread 10)',
        '(:return (:ok ((1 :direct "node.js" t))) 10)');

expect('(:write-string "Remote attached: (test) test/localhost:8080\n")');
var r1 = new FakeRemote("test/localhost:8080");
executive.attachRemote(r1);
verifyExpectations();

request('(:emacs-rex (js:list-remotes) "JS" :repl-thread 11)',
        '(:return (:ok ((1 :direct "node.js" t) (2 :test "test/localhost:8080" nil))) 11)');

expect('(:write-string "Remote attached: (test) test/localhost:9999\n")');
var r2 = new FakeRemote("test/localhost:9999");
executive.attachRemote(r2);
verifyExpectations();

request('(:emacs-rex (js:list-remotes) "JS" :repl-thread 12)',
        '(:return (:ok ((1 :direct "node.js" t) ' +
        '(2 :test "test/localhost:8080" nil) ' +
        '(3 :test "test/localhost:9999" nil))) 12)');

request('(:emacs-rex (swank:listener-eval "3 * 10\n") "JS" :repl-thread 13)',
        '(:return (:ok (:values "30")) 13)');

request('(:emacs-rex (js:select-remote 2 nil) "JS" :repl-thread 14)',
        '(:write-string "Remote selected: (test) test/localhost:8080\n")',
        '(:return (:ok nil) 14)');

request('(:emacs-rex (js:list-remotes) "JS" :repl-thread 15)',
        '(:return (:ok ((1 :direct "node.js" nil) ' +
        '(2 :test "test/localhost:8080" t) ' +
        '(3 :test "test/localhost:9999" nil))) 15)');

request('(:emacs-rex (swank:listener-eval "3 * 10\n") "JS" :repl-thread 16)',
        '(:return (:ok (:values "R:test/localhost:8080:3 * 10")) 16)');

expect('(:write-string "Remote detached: (test) test/localhost:8080\n")',
       '(:write-string "Remote selected (auto): (direct) node.js\n")');
r1.disconnect();
verifyExpectations();

request('(:emacs-rex (swank:listener-eval "3 * 10\n") "JS" :repl-thread 17)',
        '(:return (:ok (:values "30")) 17)');

// TBD: add higher-level functions for testing remotes

request('(:emacs-rex (js:list-remotes) "JS" :repl-thread 18)',
        '(:return (:ok ((1 :direct "node.js" t) ' +
        '(3 :test "test/localhost:9999" nil))) 18)');

expect('(:write-string "Remote detached: (test) test/localhost:9999\n")');
r2.disconnect();
verifyExpectations();

request('(:emacs-rex (swank:listener-eval "3 * 10\n") "JS" :repl-thread 19)',
        '(:return (:ok (:values "30")) 19)');

request('(:emacs-rex (js:list-remotes) "JS" :repl-thread 20)',
        '(:return (:ok ((1 :direct "node.js" t))) 20)');

request('(:emacs-rex (js:select-remote 2 nil) "JS" :repl-thread 21)',
        '(:write-string "WARNING: bad remote index\n")',
        '(:return (:ok nil) 21)');

request('(:emacs-rex (swank:listener-eval "3 * 10\n") "JS" :repl-thread 22)',
        '(:return (:ok (:values "30")) 22)');

request('(:emacs-rex (js:select-remote 1 nil) "JS" :repl-thread 23)',
        '(:write-string "WARNING: remote already selected: (direct) node.js\n")',
        '(:return (:ok nil) 23)');

// test sticky remote selection
expect('(:write-string "Remote attached: (test) test/localhost:8001\n")');
var r3 = new FakeRemote("test/localhost:8001");
executive.attachRemote(r3);
verifyExpectations();

request('(:emacs-rex (js:list-remotes) "JS" :repl-thread 24)',
        '(:return (:ok ((1 :direct "node.js" t) (4 :test "test/localhost:8001" nil))) 24)');

request('(:emacs-rex (js:select-remote 4 t) "JS" :repl-thread 25)',
        '(:write-string "Remote selected (sticky): (test) test/localhost:8001\n")',
        '(:return (:ok nil) 25)');

expect('(:write-string "Remote detached: (test) test/localhost:8001\n")',
       '(:write-string "Remote selected (auto): (direct) node.js\n")');
r3.disconnect();
verifyExpectations();

expect('(:write-string "Remote attached: (test) test/localhost:8001\n")',
       '(:write-string "Remote selected (auto): (test) test/localhost:8001\n")');
var r5 = new FakeRemote("test/localhost:8001");
executive.attachRemote(r5);
verifyExpectations();

request('(:emacs-rex (js:select-remote 1 nil) "JS" :repl-thread 26)',
        '(:write-string "Remote selected: (direct) node.js\n")',
        '(:return (:ok nil) 26)');

request('(:emacs-rex (js:select-remote 5 nil) "JS" :repl-thread 27)',
        '(:write-string "Remote selected: (test) test/localhost:8001\n")',
        '(:return (:ok nil) 27)');

expect('(:write-string "Remote detached: (test) test/localhost:8001\n")',
       '(:write-string "Remote selected (auto): (direct) node.js\n")');
r5.disconnect();
verifyExpectations();

expect('(:write-string "Remote attached: (test) test/localhost:8001\n")');
var r6 = new FakeRemote("test/localhost:8001");
executive.attachRemote(r6);
verifyExpectations();

// TBD: use ## instead of numbers in the tests above (request() should take care of it)
// TBD: test output from an inactive remote
// TBD: change prompt upon remote selection
// TBD: remote-based prompt (add package() to remote or something like that)
// TBD: restrict remote names received from the browser (length, chars)
// TBD: remote name should be formed on the server side using small client side id
// TBD: are out-of-order results for :emacs-rex ok? look at slime sources

/*

list/select remotes along the lines of

catching errors on the client: window.onerror
http://stackoverflow.com/questions/951791/javascript-global-error-handling
*/

// TBD: add \n to messages from remotes / executive
