// -*- mode: js2; js-run: "swank-protocol-tests.js" -*-
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
