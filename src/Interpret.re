open Common;

let empty = {variables: StringMap.empty, currLine: 0};

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

let inc_line state => {...state, currLine: state.currLine + 1};

let cmd
    (state: stateT)
    (funcs: StringMap.t functionT)
    (input: string)
    cb::(cb: stateT => err::option string => unit) => {
  let s = Stream.of_string input;
  switch (Parse.parse_ident s "") {
  | Ok i =>
    switch (StringMap.get i funcs) {
    | Some fn =>
      switch (parse_args s []) {
      | Ok args =>
        fn
          args
          state
          cb::(
            fun
            | Ok state => cb (inc_line state) err::None
            | Error e => cb state err::(Some e)
          )
      | Error e => cb state err::(Some e)
      }
    | None => cb state err::(Some ("Unknown function " ^ i ^ "."))
    }
  | Error e => cb state err::(Some e)
  }
};

let rec run_until_error
        (state: stateT)
        (funcs: StringMap.t functionT)
        (inputs: array string)
        cb::(cb: stateT => err::option string => unit) =>
  switch inputs.(state.currLine) {
  | input =>
    let s = Stream.of_string input;
    switch (Parse.parse_ident s "") {
    | Ok i =>
      switch (StringMap.get i funcs) {
      | Some fn =>
        switch (parse_args s []) {
        | Ok args =>
          fn
            args
            state
            cb::(
              fun
              | Ok state => run_until_error (inc_line state) funcs inputs ::cb
              | Error e => cb state err::(Some e)
            )
        | Error e => cb state err::(Some e)
        }
      | None => cb state err::(Some ("Unknown function " ^ i ^ "."))
      }
    | Error e => cb state err::(Some e)
    }
  | exception _ =>
    /*TODO make more specific */ cb state err::None
  };

let run_until_error state funcs (input: string) ::cb =>
  run_until_error state funcs (Array.of_list (split_char input on::'\n')) ::cb;
