// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t -*-
var swh = require("./swank-handler");
var readFromString = require("./lisp").readFromString;
var assert = require("assert");

var expected = [];
var handler = new swh.Handler(new swh.Executive({ pid: 4242 }));

handler.on(
  "response", function (response) {
    assert.ok(expected.length > 0);
    var expectedResponse = expected.shift();
    assert.equal(expectedResponse, response);
  });

function request (str) {
  for (var i = 1; i < arguments.length; ++i)
    expected.push(arguments[i]);
  handler.receive(readFromString(str));
  assert.equal(0, expected.length);
}

request('(:emacs-rex (swank:connection-info) "COMMON-LISP-USER" t 1)',
        '(:return (:ok (:lisp-implementation (:name "JS" :type "JS" :version "1.5") ' +
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

// TBD: debugger
