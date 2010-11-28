// -*- mode: js2; js-run: "user-agent-tests.js" -*-

var RX_TABLE = [
    [ /^.*(Firefox|Chrome|chromeframe|Conkeror|SeaMonkey)\/(\d+\.\d+).*$/, "$1 $2"],
    [ /^.*Firefox.*$/, "Firefox" ],
    [ /^.*Opera.*Version\/(\d+\.\d+).*$/, "Opera $1" ],
    [ /^.*Opera[/ ](\d+\.\d+).*$/, "Opera $1" ],
    [ /^.*Android (\d+\.\d+).*$/, "Android $1" ],
    [ /^.*iPhone;.*OS (\d+(?:_\d+)*).*$/, "iPhone $1" ],
    [ /^.*iPad;.*OS (\d+(?:_\d+)*).*$/, "iPad $1" ],
    [ /^.*iPod;.*$/, "iPod" ],
    [ /^.*Version\/(\d+\.\d+).*Safari.*$/, "Safari $1" ],
    [ /^.*Qt\/(\d+\.\d+).*$/, "QtWeb $1" ],
    [ /^.*WebKit.*$/, "WebKit" ],
    [ /^.*Gecko.*$/, "Gecko" ],
    [ /^.*MSIE (\d+\.\d+).*$/, "MSIE $1" ]
];

exports.recognize = function recognize (name) {
  // console.log("name=%s", name);
  for (var i = 0; i < RX_TABLE.length; ++i) {
    var r = name.replace(RX_TABLE[i][0], RX_TABLE[i][1]);
    // console.log("m=%s r=%s", RX_TABLE[i][0], r);
    if (r != name)
      return r.replace(/chromeframe/, "ChromeFrame").replace(/_/g, ".");
  }
  return "unknown";
};
