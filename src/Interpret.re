open Common;

let empty = {variables: StringMap.empty, functions: StringMap.empty};

let parse_arg stream =>
  switch (Stream.peek stream) {
  | Some '0'..'9'
  | Some '.' =>
    switch (Parse.parse_num stream "") {
    | Ok n => Ok (Val (Num n))
    | Error e => Error e
    }
  | Some 'a'..'z'
  | Some 'A'..'Z' =>
    switch (Parse.parse_ident stream "") {
    | Ok n => Ok (Var n)
    | Error e => Error e
    }
  | Some _ => Error "Unrecognized character"
  | None => Error "No arg"
  };

let rec parse_args stream acc =>
  switch (Stream.peek stream) {
  | Some ' '
  | Some '\t' =>
    Stream.junk stream;
    parse_args stream acc
  | None
  | Some '\n' => Ok (List.rev acc)
  | Some _ =>
    switch (parse_arg stream) {
    | Ok a => parse_args stream [a, ...acc]
    | Error e => Error e
    }
  };

let load_builtins = Builtins.load_builtins;

let cmd state input ::cb => {
  let s = Stream.of_string input;
  switch (Parse.parse_ident s "") {
  | Ok i =>
    switch (StringMap.get i state.functions) {
    | Some fn =>
      switch (parse_args s []) {
      | Ok args => fn args state cb::(fun state => cb state)
      | Error e =>
        print_endline e;
        cb state
      }
    | None =>
      print_endline i;
      cb state
    }
  | Error e =>
    print_endline e;
    cb state
  }
};
