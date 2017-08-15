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

let builtins_list =
  Builtins.[add, sub, mul, div, move, print (fun s => print_string s)];

let funcs = Builtins.load_builtins_list builtins_list Common.StringMap.empty;

let rec prompt state =>
  Readline.question
    rl
    "> "
    (
      fun s => {
        let s = CharStream.create s;
        let parsed = Interpret.parse_program s funcs [];
        switch parsed {
        | Ok cmds =>
          Interpret.run_until_error
            {state}
            step::false
            cb::(
              fun state ::err =>
                switch err {
                | None => prompt state
                | Some e =>
                  print_endline ("Error: " ^ e);
                  prompt state
                }
            )
        | Error e =>
          print_endline ("Error: " ^ e);
          prompt state
        }
      }
    );

prompt Interpret.empty;
