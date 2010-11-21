// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t; js-run: "swank-handler-tests.js" -*-
var EventEmitter = require("events").EventEmitter;
var Script = process.binding('evals').Script;
var evalcx = Script.runInContext;
var util = require("util");
var lisp = require("./lisp");
var S = lisp.S, consp = lisp.consp, car = lisp.car, cdr = lisp.cdr,
    repr = lisp.repr, fromLisp = lisp.fromLisp, toLisp = lisp.toLisp;

function Handler (executive) {
  this.executive = executive;
  var self = this;
  this.executive.on("output", function (str) { self.output(str); });
};

util.inherits(Handler, EventEmitter);

Handler.prototype.receive = function receive (message) {
  // FIXME: error handling
  console.log("Handler.prototype.receive(): %s", repr(message).replace(/\n/, "\\n"));
  if (!consp(message) || car(message) != S(":emacs-rex")) {
    console.log("bad message: %s", message);
    return;
  }
  var d, expr;
  try {
    d = fromLisp(message, ["S:op", ">:form",
                           ["S:name", "R*:args"],
                           "_:package", "_:threadId", "N:id"]);
  } catch (e) {
    if (e instanceof TypeError) {
      console.log("failed to parse %s: %s", message, e);
      return; // FIXME
    }
    throw e;
  }
  var r = { status: ":ok", result: null };
  switch (d.form.name) {
  case "swank:connection-info":
    r.result = toLisp(this.executive.connectionInfo(),
                      { "pid": "N:pid",
                        "package": { name: "packageSpec", spec: { name: "s", prompt: "s" } },
                        "lisp-implementation": {
                          name: "implementation",
                          spec: { type: "s", name: "s", version: "s" } } });
    break;
  case "swank:create-repl":
    r.result = toLisp(this.executive.createRepl(), ["s:packageName", "s:prompt"]);
    break;
  case "swank:autodoc":
    r.result = S(":not-available");
    break;
  case "swank:listener-eval":
    if (d.form.args.length != 1) {
      console.log("bad args len for SWANK:LISTENER-EVAL -- %s", d.form.args.length);
      return; // FIXME
    }
    try {
      expr = fromLisp(d.form.args[0], "s");
    } catch (e) {
      if (e instanceof TypeError) {
        console.log("can't parse arg -- %s", d.form.args[0]);
        return; // FIXME
      }
      throw e;
    }
    var values = this.executive.listenerEval(expr);
    if (values.length)
      r.result = toLisp({ values: values }, [S(":values"), "R:values"]);
    break;
  default:
    // FIXME: handle unknown commands
  }
  this.sendResponse({ r: r, id: d.id },
                    [S(":return"), ">:r", ["S:status", "_:result"], "N:id"]);
};

Handler.prototype.output = function output (str) {
  this.sendResponse([S(":write-string"), str]);
};

Handler.prototype.sendResponse = function sendResponse(response, spec)
{
  this.emit("response", repr(toLisp(response, spec || "@")));
};

function Executive (options) {
  options = options || {};
  this.pid = options.hasOwnProperty("pid") ? options.pid : null;
  this.setupContext();
};

util.inherits(Executive, EventEmitter);

Executive.prototype.setupContext = function setupContext () {
  this.context = Script.createContext();
  for (var i in global) this.context[i] = global[i];
  this.context.module = module;
  this.context.require = require;
  var self = this;
  this.context._swank = {
    output: function output (arg) {
      self.emit("output", "" + arg);
    }
  };
};

Executive.prototype.connectionInfo = function connectionInfo () {
  return { pid: this.pid === null ? process.pid : this.pid,
           packageSpec: { name: "JS", prompt: "JS" },
           implementation: { type: "JS", name: "JS", version: "1.5" } };
};

Executive.prototype.createRepl = function createRepl () {
  return { packageName: "JS", prompt: "JS" };
};

Executive.prototype.listenerEval = function listenerEval (str) {
  var r;
  try {
    r = evalcx(str, this.context, "repl");
  } catch (e) {
    r = undefined;
    this.emit("output", e.stack);
  }
  return r === undefined ? [] : [util.inspect(r)];
};

exports.Handler = Handler;
exports.Executive = Executive;
