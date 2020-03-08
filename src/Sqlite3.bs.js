// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Caml_option = require("bs-platform/lib/js/caml_option.js");
var BetterSqlite3 = require("better-sqlite3");

var Statement = { };

var Config = { };

function make(path, memory, readonly, verbose, fileMustExist, param) {
  var tmp = { };
  if (memory !== undefined) {
    tmp.memory = Caml_option.valFromOption(memory);
  }
  if (readonly !== undefined) {
    tmp.readonly = Caml_option.valFromOption(readonly);
  }
  if (fileMustExist !== undefined) {
    tmp.fileMustExist = Caml_option.valFromOption(fileMustExist);
  }
  if (verbose !== undefined) {
    tmp.verbose = Caml_option.valFromOption(verbose);
  }
  return new BetterSqlite3(path, tmp);
}

var Database = {
  Config: Config,
  make: make
};

var Config$1 = { };

function make$1(path, memory, readonly, verbose, fileMustExist, param) {
  var tmp = { };
  if (memory !== undefined) {
    tmp.memory = Caml_option.valFromOption(memory);
  }
  if (readonly !== undefined) {
    tmp.readonly = Caml_option.valFromOption(readonly);
  }
  if (fileMustExist !== undefined) {
    tmp.fileMustExist = Caml_option.valFromOption(fileMustExist);
  }
  if (verbose !== undefined) {
    tmp.verbose = Caml_option.valFromOption(verbose);
  }
  return new BetterSqlite3(path, tmp);
}

var Connection = {
  Config: Config$1,
  make: make$1
};

exports.Statement = Statement;
exports.Database = Database;
exports.Connection = Connection;
/* better-sqlite3 Not a pure module */