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
  this.needChars = HEADER_LEN;
  this.handleData = this.handleHeader;
  this.stash = "";
  this.onMessage = onMessage;
};

// FIXME: proper error handling (handle both packet parsing and reader errors)

SwankParser.prototype.execute = function execute (text) {
  var offset = 0;
  while (offset < text.length)
    offset += this.handleContent(text, offset);
};

SwankParser.prototype.handleContent = function handleContent (text, offset) {
  var stashLen = this.stash.length;
  var avail = Math.min(this.needChars, text.length + stashLen - offset);
  var message = this.stash + text.substring(offset, offset + avail - stashLen);
  if (avail < this.needChars)
    this.stash = message;
  else {
    this.stash = "";
    this.handleData(message);
  }
  return message.length - stashLen;
};

SwankParser.prototype.handleHeader = function handleHeader (str) {
  var count = parseInt(str, 16) || 0;
  if (count > 0 && count < MAX_MESSAGE_SIZE) {
    this.needChars = count;
    this.handleData = this.handleMessage;
  } else
    this.needChars = HEADER_LEN; // FIXME: handle errors
};

SwankParser.prototype.handleMessage = function handleMessage (str) {
  this.onMessage(readFromString(str)); // FIXME: handle errors
  this.needChars = HEADER_LEN;
  this.handleData = this.handleHeader;
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
