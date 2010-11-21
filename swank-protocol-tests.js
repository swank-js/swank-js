// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t -*-
var swp = require("./swank-protocol");
var lisp = require("./lisp");
var Buffer = require('buffer').Buffer;
var assert = require("assert");
var S = lisp.S, list = lisp.list, nil = lisp.nil;

var expected = [];

var parser = new swp.SwankParser(
  function onMessage (message) {
    assert.ok(expected.length > 0);
    var expectedMessage = expected.shift();
    assert.deepEqual(expectedMessage, message);
  });

function feed (text) {
  for (var i = 1; i < arguments.length; ++i)
    expected.push(arguments[i]);
  var buffer = new Buffer(text, "utf8");
  parser.execute(buffer);
  assert.equal(0, expected.length);
}

// dispatch: see dispatch-event in swank.lisp
feed("000");
feed("03b");
feed("(:emacs-rex (swank:connection-info) \"COMMON-LISP-USER\" t 1)",
     list(S(":emacs-rex"), list(S("swank:connection-info")),
          "COMMON-LISP-USER", S("t"), 1));

feed("0");
feed("0");
feed("0");
feed("03b(:emacs-rex (swank:connection-info)");
feed(" \"COMMON-LISP-USER\" t 1)000",
     list(S(":emacs-rex"), list(S("swank:connection-info")),
          "COMMON-LISP-USER", S("t"), 1));

feed("03b(:emacs-rex (swank:connection-info)");
feed(" \"COMMON-LISP-USER\" t 1");
feed(")",
     list(S(":emacs-rex"), list(S("swank:connection-info")),
          "COMMON-LISP-USER", S("t"), 1));

assert.equal(
  "000015(:return (:ok nil) 1)",
  swp.buildMessage(list(S(":return"), list(S(":ok"), nil), 1)).toString("utf8"));

// TBD: check unicode string handling (use \uxxxx notation)