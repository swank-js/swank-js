// -*- mode: js2 -*-
var path = require("path"), fs = require("fs");

function Config (fileName) {
  this.fileName = fileName;
  if (/^~\//.test(this.fileName))
    this.fileName = path.join(process.env.HOME || "/", this.fileName.substring(2));
  this.config = null;
}

Config.prototype.loadConfig = function loadConfig (cont) {
  var self = this;
  if (!this.config) {
    fs.readFile(
      self.fileName, "utf-8", function (err, data) {
        self.config = {};
        if (!err) {
          try {
            self.config = JSON.parse(data);
          } catch (e) {}
        }
        cont(self.config);
      });
  } else
    cont(this.config);
};

Config.prototype.saveConfig = function saveConfig (cont) {
  if (!this.config)
    return;
  var self = this;
  fs.writeFile(
    this.fileName, JSON.stringify(this.config), "utf8",
    function (err) {
      if (err)
        console.warn("error writing config file %s: %s", self.fileName, err);
      cont();
    });
};

Config.prototype.get = function get (name, cont) {
  this.loadConfig(
    function (cfg) {
      cont(cfg.hasOwnProperty(name) ? cfg[name] : undefined);
    });
};

Config.prototype.set = function set (name, value, cont) {
  var self = this;
  cont = cont || function () {};
  this.loadConfig(
    function (cfg) {
      cfg[name] = value;
      self.saveConfig(cont);
    });
};

function FakeConfig (values) {
  this.config = values || {};
}

FakeConfig.prototype.getNow = function getNow (name) {
  return this.config.hasOwnProperty(name) ? this.config[name] : undefined;
};

FakeConfig.prototype.get = function get (name, cont) {
  cont(this.config.hasOwnProperty(name) ? this.config[name] : undefined);
};

FakeConfig.prototype.set = function set (name, value, cont) {
  this.config[name] = value;
  if (cont) cont();
};

exports.Config = Config;
exports.FakeConfig = FakeConfig;
