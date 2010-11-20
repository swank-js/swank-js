// -*- mode: js2; moz-minor-mode: nil; jsc-minor-mode: t -*-
var rd = require("./reader");
var assert = require("assert");

assert.equal(rd.S("zzz"), rd.S("zzz"));
assert.deepEqual(rd.cons(1, rd.cons(2, rd.cons(3, rd.nil))), rd.list(1, 2, 3));
assert.equal("abc", rd.car(rd.cons("abc", "def")));
assert.equal("def", rd.cdr(rd.cons("abc", "def")));
assert.equal(rd.nil, rd.list());
assert.ok(rd.consp(rd.cons(1, 2)));
assert.ok(!rd.consp(rd.nil));
assert.ok(rd.listp(rd.cons(1, 2)));
assert.ok(rd.listp(rd.list(1, 2)));
assert.ok(rd.listp(rd.nil));
assert.ok(rd.nullp(rd.nil));
assert.ok(!rd.nullp(rd.cons(1, 2)));
assert.ok(!rd.nullp(1));
assert.deepEqual(rd.list(), rd.reverse(rd.list()));
assert.deepEqual(rd.list(1), rd.reverse(rd.list(1)));
assert.deepEqual(rd.list(3, 2, 1), rd.reverse(rd.list(1, 2, 3)));

var s = new rd.StringInputStream("abc");
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

s = new rd.StringInputStream("");
assert.equal(0, s.pos());
assert["throws"](function () { s.ungetc("z"); });
assert["throws"](function () { s.ungetc(""); });
assert.equal(null, s.getc());
assert["throws"](function () { s.readchar(); });
assert.equal(0, s.pos());

function test_read (str, o) {
  assert.equal(str, rd.repr(o));
  var rs = rd.readFromString(str);
  assert.deepEqual(o, rs);
  assert.equal(str, rd.repr(rs));
};

test_read("ZZZ", rd.S("zzz"));
test_read("'ZZZ", rd.list(rd.S("quote"), rd.S("zzz")));
test_read('"zzz"', "zzz");
test_read('\'"zzz"', rd.list(rd.S("quote"), "zzz"));
test_read("()", rd.nil);
test_read("(1)", rd.list(1));
test_read("(1 2)", rd.list(1, 2));
test_read("(1 2 EPRST)", rd.list(1, 2, rd.S("eprst")));
test_read('(1 2 EPRST ("abra" "kodabra"))',
          rd.list(1, 2, rd.S("eprst"), rd.list("abra", "kodabra")));
test_read('(1 2 EPRST ("abra" . "kodabra"))',
          rd.list(1, 2, rd.S("eprst"), rd.cons("abra", "kodabra")));
test_read('(1 2 EPRST ("abra" "kodabra" .SCHWABBRA))',
          rd.list(1, 2, rd.S("eprst"), rd.list("abra", "kodabra", rd.S(".schwabbra"))));
test_read('(1 2 EPRST ("abra" "kodabra" .SCHWABBRA . QQQ))',
          rd.list(1, 2, rd.S("eprst"),
                  rd.cons("abra",rd.cons("kodabra", rd.cons(rd.S(".schwabbra"), rd.S("QQQ"))))));
test_read('(1 2 EPRST \'("abra" "kodabra" .SCHWABBRA . QQQ))',
          rd.list(1, 2, rd.S("eprst"),
                  rd.list(rd.S("quote"),
                          rd.cons("abra", rd.cons("kodabra", rd.cons(rd.S(".schwabbra"), rd.S("QQQ")))))));
test_read("(1 2 3)", rd.list(1, 2, 3));
test_read("(1 2 3 (4 5 6))", rd.list(1, 2, 3, rd.list(4, 5, 6)));
test_read("((4 5 6) . 7)", rd.cons(rd.list(4, 5, 6), 7));
test_read("((4 5 6) 7 8 . :EPRST)",
          rd.cons(rd.list(4, 5, 6), rd.cons(7, rd.cons(8, rd.S(":EPRST")))));

// function location determination:
// for code loaded from scripts: direct (if possible)
// for code entered via C-M-x etc.: use some kind of translation scheme
