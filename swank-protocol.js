// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t; js-run: "swank-protocol-tests.js" -*-
var readFromString = require("./lisp").readFromString;

const HEADER_LEN = 6;
const DUMMY_HEADER = "000000";
const MAX_MESSAGE_SIZE = 10 * 1024 * 1024;

function SwankParser (onMessage) {
  this.needBytes = HEADER_LEN;
  this.handleData = this.handleHeader;
  this.stash = null;
  this.onMessage = onMessage;
};

// FIXME: proper error handling (handle both packet parsing and reader errors)

SwankParser.prototype.execute = function execute (buffer) {
  var offset = 0;
  while (offset < buffer.length)
    offset += this.handleContent(buffer, offset);
};

SwankParser.prototype.handleContent = function handleContent (buffer, offset) {
  var stashLen = this.stash ? this.stash.length : 0;
  var avail = Math.min(this.needBytes, buffer.length + stashLen - offset);
  var messageBuffer = new Buffer(avail);
  if (this.stash)
    this.stash.copy(messageBuffer, 0, 0);
  buffer.copy(messageBuffer, stashLen, offset);
  if (avail < this.needBytes)
    this.stash = messageBuffer;
  else {
    this.stash = null;
    var messageStr = messageBuffer.toString();
    this.handleData(messageStr);
  }
  return messageBuffer.length - stashLen;
};

SwankParser.prototype.handleHeader = function handleHeader (str) {
  var count = parseInt(str, 16) || 0;
  if (count > 0 && count < MAX_MESSAGE_SIZE) {
    this.needBytes = count;
    this.handleData = this.handleMessage;
  } else
    this.needBytes = HEADER_LEN; // FIXME: handle errors
};

SwankParser.prototype.handleMessage = function handleMessage (str) {
  this.onMessage(readFromString(str)); // FIXME: handle errors
  this.needBytes = HEADER_LEN;
  this.handleData = this.handleHeader;
};

function buildMessage (obj) {
  var buf = new Buffer(DUMMY_HEADER + obj.toString());
  var lenStr = "" + (buf.length - HEADER_LEN).toString(16);
  for (var i = 0; i < lenStr.length; ++i)
    buf[i + HEADER_LEN - lenStr.length] = lenStr.charCodeAt(i);
  return buf;
};

exports.SwankParser = SwankParser;
exports.buildMessage = buildMessage;
