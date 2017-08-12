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
  | Some '"' =>
    Stream.junk stream;
    switch (Parse.parse_string stream "") {
    | Ok n => Ok (Val (Str n))
    | Error e => Error e
    }
  | Some 'a'..'z'
  | Some 'A'..'Z' =>
    switch (Parse.parse_ident stream "") {
    | Ok n => Ok (Var n)
    | Error e => Error e
    }
  | Some c => Error (Parse.append_char "Unrecognized character: " c)
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

let cmd (state: stateT) (input: string) cb::(cb: result stateT string => unit) => {
  let s = Stream.of_string input;
  print_endline input;
  switch (Parse.parse_ident s "") {
  | Ok i =>
    switch (StringMap.get i state.functions) {
    | Some fn =>
      switch (parse_args s []) {
      | Ok args => fn args state cb::(fun state => cb state)
      | Error e => cb (Error e)
      }
    | None => cb (Error ("Unknown function " ^ i ^ "."))
    }
  | Error e => cb (Error e)
  }
};

let rec run_until_error
        (state: stateT)
        (input: list string)
        cb::(cb: result stateT string => unit) =>
  switch input {
  | [] => cb (Ok state)
  | ["", ...tl] => run_until_error state tl ::cb
  | [input, ...tl] =>
    let s = Stream.of_string input;
    print_endline input;
    switch (Parse.parse_ident s "") {
    | Ok i =>
      switch (StringMap.get i state.functions) {
      | Some fn =>
        switch (parse_args s []) {
        | Ok args =>
          fn
            args
            state
            cb::(
              fun
              | Ok state => run_until_error state tl ::cb
              | Error e => cb (Error e)
            )
        | Error e => cb (Error e)
        }
      | None => cb (Error ("Unknown function " ^ i ^ "."))
      }
    | Error e => cb (Error e)
    }
  };
