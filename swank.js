// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t -*-

var assert = require("assert");

var I = {};

function _symbol (name) {
  this.name = name;
}

_symbol.prototype.toString = function toString () {
  return this.name.toUpperCase();
};

function S(name) {
  name = name.toUpperCase();
  if (I.hasOwnProperty(name))
    return I[name];
  return I[name] = new _symbol(name);
};

var nil = S("NIL");

nil.toString = function () {
  return "()";
};

function nullp (o) {
  return o === nil;
};

function _cons (car, cdr) {
  this.car = car;
  this.cdr = cdr;
}

_cons.prototype.toString = function toString () {
  var result = [];
  if (this.car == S("quote") && consp(this.cdr) && nullp(this.cdr.cdr))
    return "'" + repr(this.cdr.car);
  for (var c = this;; c = c.cdr) {
    if (consp(c))
      result.push(repr(c.car));
    else {
      if (!nullp(c))
        result.push(". " + repr(c));
      break;
    }
  }
  return '(' + result.join(" ") + ')';
};

function consp (o) {
  return o instanceof _cons;
}

function cons (car, cdr) {
  return new _cons(car, cdr);
}

function car (o) {
  return o.car;
}

function cdr (o) {
  return o.cdr;
}

function repr (x) {
  if (typeof(x) == "string")
    return '"' + x.replace(/"/g, '\\"') + '"';
  return String(x);
};

function list () {
  var tail = nil;
  for (var i = arguments.length - 1; i >= 0; --i)
    tail = cons(arguments[i], tail);
  return tail;
}

function listp (o) {
  return nullp(o) || consp(o);
}

function reverse (l) {
  var r = nil;
  for (; !nullp(l); l = cdr(l))
    r = cons(car(l), r);
  return r;
}

function StringInputStream (string) {
  this._string = string;
  this._pos = 0;
  this._max = this._string.length;
}

StringInputStream.prototype.pos = function pos () {
  return this._pos;
};

StringInputStream.prototype.getc = function getc () {
  if (this._pos == this._max)
    return null;
  return this._string.charAt(this._pos++);
};

StringInputStream.prototype.readchar = function readchar () {
  var c = this.getc();
  if (c === null)
    throw new Error("StringInputStream.readchar(): EOF reached");
  return c;
};

StringInputStream.prototype.ungetc = function ungetc (c) {
  if (this._pos > 0 && this._string[this._pos - 1] == c)
    --this._pos;
  else { /* FIXME: { is just to make nodejs repl happy */
    throw new Error("StringInputStream.ungetc(): invalid argument");
  }
};

function LispReader (s) {
  this.s = s;
}

LispReader.prototype.readNumberOrSymbol = function readNumberOrSymbol () {
  var token = this.readToken();
  if (token == "")
    throw new Error("LispReader.readNumberOrSymbol(): EOF reached");
  if (/^[-+]?[0-9]+$/.test(token))
    return parseInt(token);
  if (/^[-+]?[0-9]*\.?[0-9]+(?:[dDeE][-+]?[0-9]+)?/.test(token))
    return parseFloat(token.replace(/d/g, "e"));
  return S(token);
};

LispReader.prototype.read = function read () {
  this.skipWhitespace();
  var c = this.s.getc();
  switch (c) {
  case "(":
    return this.readList();
  case '"':
    return this.readString();
  case "'":
    return this.readQuote();
  case null:
    throw new Error("LispReader.read(): EOF reached");
  default:
    this.s.ungetc(c);
    return this.readNumberOrSymbol();
  }
};

LispReader.prototype.readList = function readList () {
  var l = nil;
  var head = nil;
  while (true) {
    this.skipWhitespace();
    var c = this.s.readchar();
    switch (c) {
    case ")":
      return l;
    case ".":
      var c1 = this.s.readchar();
      if (" \n\t".indexOf(c1) < 0)
        this.s.ungetc(c1); // process the default case
      else {
        if (nullp(l))
          throw new Error("LispReader.readList(): invalid placement of the dot");
        head.cdr = this.read();
        return l;
      }
    default:
      this.s.ungetc(c);
      if (nullp(l)) {
        l = list(this.read());
        head = l;
      } else {
        head.cdr = list(this.read());
        head = head.cdr;
      }
    }
  }
  return null; /* never get there */
};

LispReader.prototype.readString = function readString () {
  var r = [];
  while (true) {
    var c = this.s.readchar();
    switch (c) {
    case '"':
      return r.join("");
    case "\\":
      c = this.s.readchar();
      if (c != "\\" && c != '"')
        throw new Error("Invalid escape char " + c);
    }
    r.push(c);
  }
  return null; /* never get there */
};

LispReader.prototype.readQuote = function readQuote () {
  return list(S("quote"), this.read());
};

LispReader.prototype.readToken = function readToken () {
  var c, token = [];
  while ((c = this.s.getc()) != null) {
    if (this.isTerminating(c)) {
      this.s.ungetc(c);
      break;
    }
    token.push(c);
  }
  return token.join("");
};

LispReader.prototype.skipWhitespace = function skipWhitespace () {
  while (true) {
    var c = this.s.getc();
    switch (c) {
    case " ":
    case "\n":
    case "\t":
      continue;
    case null:
      return;
    default:
      this.s.ungetc(c);
      return;
    }
  }
};

LispReader.prototype.isTerminating = function isTerminating (c) {
  return " \n\t()\"'".indexOf(c) >= 0;
};

function readFromString (str) {
  return new LispReader(new StringInputStream(str)).read();
}

assert.equal(S("zzz"), S("zzz"));
assert.deepEqual(cons(1, cons(2, cons(3, nil))), list(1, 2, 3));
assert.equal("abc", car(cons("abc", "def")));
assert.equal("def", cdr(cons("abc", "def")));
assert.equal(nil, list());
assert.ok(consp(cons(1, 2)));
assert.ok(!consp(nil));
assert.ok(listp(cons(1, 2)));
assert.ok(listp(list(1, 2)));
assert.ok(listp(nil));
assert.ok(nullp(nil));
assert.ok(!nullp(cons(1, 2)));
assert.ok(!nullp(1));
assert.deepEqual(list(), reverse(list()));
assert.deepEqual(list(1), reverse(list(1)));
assert.deepEqual(list(3, 2, 1), reverse(list(1, 2, 3)));

var s = new StringInputStream("abc");
assert.equal(0, s.pos());
assert.equal("a", s.getc());
assert.equal(1, s.pos());
assert.equal("b", s.readchar());
assert.equal(2, s.pos());
assert.equal("c", s.readchar());
assert.equal(3, s.pos());
assert.equal(null, s.getc());
assert.equal(3, s.pos());
assert["throws"](function () { s.readchar(); });
assert.equal(3, s.pos());
s.ungetc("c");
assert.equal(2, s.pos());
assert.equal("c", s.getc());
assert.equal(3, s.pos());
assert["throws"](function () { s.ungetc("z"); });
assert.equal(3, s.pos());
s.ungetc("c");
s.ungetc("b");
assert.equal(1, s.pos());
assert.equal("b", s.getc());
assert.equal("c", s.getc());
assert.equal(3, s.pos());
s.ungetc("c");
s.ungetc("b");
s.ungetc("a");

assert.equal(0, s.pos());
assert["throws"](function () { s.ungetc("z"); });
assert["throws"](function () { s.ungetc(""); });
assert.equal(0, s.pos());
assert.equal("a", s.readchar());
assert.equal("b", s.readchar());
assert.equal("c", s.readchar());
assert.equal(3, s.pos());

s = new StringInputStream("");
assert.equal(0, s.pos());
assert["throws"](function () { s.ungetc("z"); });
assert["throws"](function () { s.ungetc(""); });
assert.equal(null, s.getc());
assert["throws"](function () { s.readchar(); });
assert.equal(0, s.pos());

function test_read (str, o) {
  assert.equal(str, repr(o));
  var r = readFromString(str);
  assert.deepEqual(o, r);
  assert.equal(str, repr(r));
};

test_read("ZZZ", S("zzz"));
test_read("'ZZZ", list(S("quote"), S("zzz")));
test_read('"zzz"', "zzz");
test_read('\'"zzz"', list(S("quote"), "zzz"));
test_read("()", nil);
test_read("(1)", list(1));
test_read("(1 2)", list(1, 2));
test_read("(1 2 EPRST)", list(1, 2, S("eprst")));
test_read('(1 2 EPRST ("abra" "kodabra"))',
          list(1, 2, S("eprst"), list("abra", "kodabra")));
test_read('(1 2 EPRST ("abra" . "kodabra"))',
          list(1, 2, S("eprst"), cons("abra", "kodabra")));
test_read('(1 2 EPRST ("abra" "kodabra" .SCHWABBRA))',
          list(1, 2, S("eprst"), list("abra", "kodabra", S(".schwabbra"))));
test_read('(1 2 EPRST ("abra" "kodabra" .SCHWABBRA . QQQ))',
          list(1, 2, S("eprst"),
               cons("abra",cons("kodabra", cons(S(".schwabbra"), S("QQQ"))))));
test_read('(1 2 EPRST \'("abra" "kodabra" .SCHWABBRA . QQQ))',
          list(1, 2, S("eprst"),
               list(S("quote"),
                    cons("abra", cons("kodabra", cons(S(".schwabbra"), S("QQQ")))))));
test_read("(1 2 3)", list(1, 2, 3));
test_read("(1 2 3 (4 5 6))", list(1, 2, 3, list(4, 5, 6)));
test_read("((4 5 6) . 7)", cons(list(4, 5, 6), 7));
test_read("((4 5 6) 7 8 . :EPRST)",
          cons(list(4, 5, 6), cons(7, cons(8, S(":EPRST")))));
