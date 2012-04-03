// -*- mode: js2; js-run: "swank-protocol-tests.js" -*-
//
// Copyright (c) 2010 Ivan Shvedunov. All rights reserved.
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

var readFromString = require("./lisp").readFromString;

const HEADER_LEN = 6;
const DUMMY_HEADER = "000000";
const MAX_MESSAGE_SIZE = 10 * 1024 * 1024;

function SwankParser (onMessage) {
  this.onMessage = onMessage;
};

// FIXME: proper error handling (handle both packet parsing and reader errors)
SwankParser.prototype.execute = function execute (buffer) {
  var stash = buffer.toString();
  var count = parseInt(stash.substr(0, HEADER_LEN), 16) || 0;
  var message = stash.substr(HEADER_LEN, HEADER_LEN + count);
  this.handleMessage(message);
};

SwankParser.prototype.handleMessage = function handleMessage (str) {
  this.onMessage(readFromString(str));
};

function buildMessage (obj) {
  var str = obj.toString();
  var lenStr = "" + str.length.toString(16);
  while (lenStr.length < HEADER_LEN)
    lenStr = "0" + lenStr;
  return lenStr + str;
};

exports.SwankParser = SwankParser;
exports.buildMessage = buildMessage;
