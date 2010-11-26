// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t; js-run: "swank-handler-tests.js" -*-
var EventEmitter = require("events").EventEmitter;
var Script = process.binding('evals').Script;
var evalcx = Script.runInContext;
var util = require("util");
var assert = process.assert;
var lisp = require("./lisp");
var S = lisp.S, list = lisp.list, consp = lisp.consp, car = lisp.car, cdr = lisp.cdr,
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
  var self = this;
  var cont = function cont () {
    self.sendResponse({ r: r, id: d.id },
                      [S(":return"), ">:r", ["S:status", "_:result"], "N:id"]);
  };

  switch (d.form.name) {
  case "swank:connection-info":
    r.result = toLisp(this.executive.connectionInfo(),
                      { "pid": "N:pid",
                        "encoding": { name: "encoding", spec: { "coding-system": "s:codingSystem",
                                                                "external-format": "s:externalFormat" } },
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
  case "js:list-remotes":
    // FIXME: support 'list of similar elements' type spec
    r.result = toLisp(
      this.executive.listRemotes().map(
        function (item) {
          return toLisp(item, ["N:index", "K:kind", "s:id", "B:isActive"]);
        }, this));
    break;
  case "js:select-remote":
    if (d.form.args.length != 2) {
      console.log("bad args len for SWANK:SELECT-REMOTE -- %s", d.form.args.length);
      return; // FIXME
    }
    var remoteIndex, sticky;
    try {
      // FIXME: args should be a cons / NIL
      remoteIndex = fromLisp(d.form.args[0], "N");
      sticky = fromLisp(d.form.args[1]);
    } catch (e) {
      if (e instanceof TypeError) {
        console.log("can't parse arg -- %s", d.form.args[0]);
        return; // FIXME
      }
      throw e;
    }
    this.executive.selectRemote(remoteIndex, sticky);
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
    this.executive.listenerEval(
      expr, function (values) {
        if (values.length)
          r.result = toLisp({ values: values }, [S(":values"), "R:values"]);
        cont();
      });
    return;
  default:
    // FIXME: handle unknown commands
  }
  cont();
};

Handler.prototype.output = function output (str) {
  this.sendResponse([S(":write-string"), str]);
};

Handler.prototype.sendResponse = function sendResponse(response, spec)
{
  this.emit("response", repr(toLisp(response, spec || "@")));
};

function Remote () {};

util.inherits(Remote, EventEmitter);

Remote.prototype.kind = function kind () {
  throw new Error("must override Remote.prototype.kind()");
};

Remote.prototype.id = function id () {
  throw new Error("must override Remote.prototype.id()");
};

Remote.prototype.evaluate = function evaluate (id, str) {
  throw new Error("must override Remote.prototype.evaluate()");
};

Remote.prototype.fullName = function fullName () {
  return "(" + this.kind() + ") " + this.id();
};

Remote.prototype.disconnect = function disconnect () {
  this.emit("disconnect");
};

Remote.prototype.detachSelf = function detachSelf () {
  this.removeAllListeners("output");
  this.removeAllListeners("disconnect");
  this.removeAllListeners("result");
};

Remote.prototype.output = function output (str) {
  this.emit("output", String(str));
};

Remote.prototype.setIndex = function setIndex (n) {
  this._index = n;
};

Remote.prototype.index = function index () {
  return this._index;
};

Remote.prototype.sendResult = function sendResult (id, values) {
  this.emit("result", id, values);
};

function DefaultRemote () {
  this.context = Script.createContext();
  for (var i in global) this.context[i] = global[i];
  this.context.module = module;
  this.context.require = require;
  var self = this;
  this.context._swank = {
    output: function output (arg) {
      self.output(arg);
    }
  };
}

util.inherits(DefaultRemote, Remote);

DefaultRemote.prototype.kind = function kind () {
  return "direct";
};

DefaultRemote.prototype.id = function id () {
  return "node.js";
};

DefaultRemote.prototype.evaluate = function evaluate (id, str) {
  var r;
  try {
    r = evalcx(str, this.context, "repl");
  } catch (e) {
    r = undefined;
    this.output(e.stack);
  }
  this.sendResult(id, r === undefined ? [] : [util.inspect(r)]);
};

// TBD: rename Executive to Dispatcher
function Executive (options) {
  options = options || {};
  this.pid = options.hasOwnProperty("pid") ? options.pid : null;
  this.remotes = [];
  this.attachRemote(new DefaultRemote());
  this.activeRemote = this.remotes[0];
  this.stickyRemoteFullName = null;
  this.pendingRequests = {};
};

util.inherits(Executive, EventEmitter);

Executive.nextId = 1; // request id counter is global in order to avoid inter-connection conflicts

Executive.nextRemoteIndex = 1;

Executive.prototype.attachRemote = function attachRemote (remote) {
  assert(this.remotes.indexOf(remote) < 0);
  remote.setIndex(Executive.nextRemoteIndex++);

  var self = this;
  remote.on(
    "output", function (str) {
      if (remote == self.activeRemote)
        self.emit("output", str);
    });
  remote.on(
    "disconnect", function (str) {
      self.handleDisconnectRemote(remote);
    });
  remote.on(
    "result", function (id, values) {
      if (!self.pendingRequests[id]) {
        self.emit("output", "WARNING: received late result from " + remote.fullName() + "\n");
        return;
      }
      try {
        self.pendingRequests[id](values);
      } finally {
        delete self.pendingRequests[id];
      }
    });
  this.remotes.push(remote);
  this.emit("output", "Remote attached: " + remote.fullName() + "\n");
  if (this.stickyRemoteFullName !== null &&
      (!this.activeRemote ||
       this.activeRemote.fullName() != this.stickyRemoteFullName) &&
      remote.fullName() == this.stickyRemoteFullName)
    this.selectRemote(remote.index(), true, true);
};

Executive.prototype.handleDisconnectRemote = function handleDisconnectRemote (remote) {
  remote.detachSelf();
  var index = this.remotes.indexOf(remote);
  if (index < 0) {
    this.emit("output", "WARNING: disconnectRemote() called for an unknown remote: " + remote.fullName() + "\n");
    return;
  }
  this.remotes.splice(index, 1);
  this.emit("output", "Remote detached: " + remote.fullName() + "\n");
  if (remote == this.activeRemote)
    this.selectRemote(this.remotes[0].index(), false, true);
};

Executive.prototype.connectionInfo = function connectionInfo () {
  return { pid: this.pid === null ? process.pid : this.pid,
           encoding: { codingSystem: "utf-8", externalFormat: "UTF-8" },
           packageSpec: { name: "JS", prompt: "JS" },
           implementation: { type: "JS", name: "JS", version: "1.5" } };
};

Executive.prototype.createRepl = function createRepl () {
  return { packageName: "JS", prompt: "JS" };
};

Executive.prototype.listenerEval = function listenerEval (str, cont) {
  var id = Executive.nextId++;
  this.pendingRequests[id] = cont;
  this.activeRemote.evaluate(id, str);
};

Executive.prototype.listRemotes = function listRemotes () {
  return this.remotes.map(
    function (remote) {
      return { index: remote.index(), kind: remote.kind(), id: remote.id(),
               isActive: remote === this.activeRemote };
    }, this);
};

Executive.prototype.selectRemote = function selectRemote (index, sticky, auto) {
  // TBD: sticky support (should autoselect the remote with message upon attachment)
  for (var i = 0; i < this.remotes.length; ++i) {
    var remote = this.remotes[i];
    if (remote.index() == index) {
      if (remote == this.activeRemote) {
        this.emit("output", "WARNING: remote already selected: " + remote.fullName() + "\n");
        return;
      }
      this.activeRemote = remote;
      if (!auto)
        this.stickyRemoteFullName = sticky ? remote.fullName() : null;
      this.emit("output", "Remote selected" + (auto ? " (auto)" : sticky ? " (sticky)" : "") +
                ": " + remote.fullName() + "\n");
      return;
    }
  }
  this.emit("output", "WARNING: bad remote index\n");
};

exports.Handler = Handler;
exports.Remote = Remote;
exports.Executive = Executive;
