type errorT;

external readFile :
  string =>
  _ [@bs.as "utf8"] =>
  (errorT => Js.Undefined.t string => unit) =>
  unit =
  "" [@@bs.module "fs"];

module Readline = {
  type pipeT;
  type interfaceDefT;
  type interfaceT;
  external createInterfaceDef : input::pipeT => output::pipeT => interfaceDefT =
    "" [@@bs.obj];
  external createInterface : interfaceDefT => interfaceT =
    "createInterface" [@@bs.module "readline"];
  external question : interfaceT => string => (string => unit) => unit =
    "question" [@@bs.send];
  external stdin : pipeT = "process.stdin" [@@bs.val];
  external stdout : pipeT = "process.stdout" [@@bs.val];
};

let rlDef =
  Readline.createInterfaceDef input::Readline.stdin output::Readline.stdout;

let rl = Readline.createInterface rlDef;

Random.self_init ();

let rec prompt state =>
  Readline.question
    rl "> " (fun s => Interpret.cmd state s cb::(fun state => prompt state));

let state = Interpret.load_builtins Interpret.empty;

prompt state;
