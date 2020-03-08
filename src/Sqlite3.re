/**
 * https://github.com/JoshuaWise/better-sqlite3/blob/master/docs/api.md
 */
module Statement = {
  type t;

  [@bs.send] external get: (t, unit) => 'a = "get";

  [@bs.send] external getWithArgs: (t, 'a) => 'b = "get";

  [@bs.send] external all: (t, unit) => 'a = "all";

  [@bs.send] external run: (t, unit) => 'a = "run";

  [@bs.send] external runWithArgs: (t, 'a) => 'b = "run";
};

module Database = {
  type t;
module Config = {
    type t;

    [@bs.obj]
    external make:
      (
        ~memory: bool=?,
        ~readonly: bool=?,
        ~fileMustExist: bool=?,
        ~verbose: 'a => unit=?,
        unit
      ) =>
      t;
  };

  [@bs.new] [@bs.module]
  external makeDatabase: (string, Config.t) => t = "better-sqlite3";
  let make = (~path, ~memory=?, ~readonly=?, ~verbose=?, ~fileMustExist=?, _) =>
    makeDatabase(path, Config.make(~memory?, ~readonly?,~verbose?, ~fileMustExist?, ()));
  [@bs.send] external prepare: (t, string) => Statement.t = "prepare";
  [@bs.send] external transaction: (t, 'a => unit, 'a) => unit = "transaction";
  [@bs.send]
  external deffered: (t, 'a => unit, 'a) => unit = "transaction.deffered";
  // need to make a function that takes a tuple here. unimplemented
  [@bs.send]
  external transactionMany: (t, 'a => unit, 'a) => unit = "transaction";

  [@bs.send] external exec: (t, string) => t = "exec";

  [@bs.send] external close: (t, unit) => t = "close";
};
module Connection = {
  type t;

  module Config = {
    type t;

    [@bs.obj]
    external make:
      (
        ~memory: bool=?,
        ~readonly: bool=?,
        ~fileMustExist: bool=?,
        ~verbose: 'a => unit=?,
        unit
      ) =>
      t;
  };

  [@bs.module] [@bs.new]
  external connect: (string, Config.t) => t = "better-sqlite3";

  [@bs.send] external close: t => unit ="close";

  let make = (~path, ~memory=?, ~readonly=?, ~verbose=?, ~fileMustExist=?, _) =>
    connect(path, Config.make(~memory?, ~readonly?,~verbose?, ~fileMustExist?, ()));

  [@bs.send] external prepare: (t, string) => Statement.t = "prepare";

  [@bs.get] external is_open: t => bool = "open";

  [@bs.get] external in_transaction: t => bool = "inTransaction";

  [@bs.get] external name: t => string = "name";

  [@bs.get] external memory: t => bool="memory";

  [@bs.get] external readonly: t => bool="readonly";
};
