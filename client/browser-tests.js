// -*- mode: js2 -*-
//
// Copyright (c) 2012 Robert Krahn. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//
// * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following
// disclaimer in the documentation and/or other materials
// provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

function addObjectToWindow(nameInWindow, obj, callback) {
  window[nameInWindow] = obj;
  try {
    callback();
  } finally {
    delete window[nameInWindow];
  }
}

function assert(val, msg) {
  if (!val) throw new Error(msg || "Assertion failed");
}

assert.equal = function equal(expected, actual, msg) {
  assert(expected == actual, "Assertion failed: '" + actual + "' is not '" + expected + "' " + msg);
};

var Tests = {
  run: function() {
    for (var name in this) {
      if (this.hasOwnProperty(name) && name.match(/^test/)) {
        console.log("Running " + name);
        try {
          this[name]();
        } catch (e) {
          console.error("Test " + name + " not successful: " + e);
        }
      }
    }
    var msg = "All tests run OK!";
    console.log(msg);
    return msg;
  },
  testCompletionOfTopLevelString: function() {
    // "foooTestTopLev" -> ["foooTestTopLevelString"]
    var name = "foooTestTopLevelString";
    addObjectToWindow(name, {}, function() {
      var result = SwankJS.doCompletion("foooTestTopLev");
      assert.equal(1, result.length, "result.length");
      assert.equal(name, result[0], "Result wrong?");
    });
  },
  testCompletionOfProperty: function() {
    // "testPropCompletion.foo.b" -> ["testPropCompletion.foo.bar"]
    var name = "testPropCompletion";
    addObjectToWindow(name, {foo: {bar: {}}}, function() {
      var result = SwankJS.doCompletion("testPropCompletion.foo.b");
      assert.equal(1, result.length, "result.length");
      assert.equal("bar", result[0], "Result wrong?");
    });
  },
  testCompletionOfEverything: function() {
    // "testCompleteEverything." -> ["testCompleteEverything.foo", "testCompleteEverything.bar"]
    var name = "testCompleteEverything";
    addObjectToWindow(name, {foo: {}, bar: {}}, function() {
      var result = SwankJS.doCompletion("testCompleteEverything.");
      assert.equal(2, result.length, "result.length");
      assert("foo", result[0], "Result 1 wrong? " + result);
      assert("bar", result[1], "Result 2 wrong? " + result);
    });
  }
};

Tests.run();