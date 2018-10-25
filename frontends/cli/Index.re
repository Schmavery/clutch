type errorT;

[@bs.module "fs"]
external readFile:
  (string, [@bs.as "utf8"] _, (errorT, Js.Undefined.t(string)) => unit) =>
  unit =
  "";

module Readline = {
  type pipeT;
  type interfaceDefT;
  type interfaceT;
  [@bs.obj]
  external createInterfaceDef: (~input: pipeT, ~output: pipeT) => interfaceDefT =
    "";
  [@bs.module "readline"]
  external createInterface: interfaceDefT => interfaceT = "createInterface";
  [@bs.send]
  external question: (interfaceT, string, string => unit) => unit = "question";
  [@bs.val] external stdin: pipeT = "process.stdin";
  [@bs.val] external stdout: pipeT = "process.stdout";
};

let rlDef =
  Readline.createInterfaceDef(~input=Readline.stdin, ~output=Readline.stdout);

let rl = Readline.createInterface(rlDef);

Random.self_init();

let builtins_list =
  Builtins.[add, sub, mul, div, move, print(s => print_string(s))];

let funcs =
  Builtins.load_builtins_list(builtins_list, Common.StringMap.empty);

let rec prompt = state =>
  Readline.question(
    rl,
    "> ",
    s => {
      let s = CharStream.create(s);
      let parsed = Interpret.parse_program(s, funcs, []);
      switch (parsed) {
      | Ok(cmds) =>
        Interpret.run_until_error(state, ~step=false, ~cb=(state, ~err) =>
          switch (err) {
          | None => prompt(state)
          | Some(e) =>
            print_endline("Error: " ++ e);
            prompt(state);
          }
        )
      | Error(e) =>
        print_endline("Error: " ++ e);
        prompt(state);
      };
    },
  );

prompt(Interpret.empty);
