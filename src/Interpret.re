open Common;

let empty = {variables: StringMap.empty, currLine: 0, content: [||]};

let parse_arg stream =>
  switch (CharStream.peek stream) {
  | Some '0'..'9'
  | Some '.' =>
    switch (Parse.parse_num stream "") {
    | Ok n => Ok (Val (Num n))
    | Error e => Error e
    }
  | Some '"' =>
    CharStream.junk stream;
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
  switch (CharStream.peek stream) {
  | Some ' ' =>
    CharStream.eat_spaces stream;
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

let rec parse_program
        (program: CharStream.t)
        funcs
        (acc: list cmdT)
        :result (array cmdT) string =>
  switch (CharStream.peek program) {
  | Some 'a'..'z'
  | Some 'A'..'Z' =>
    switch (Parse.parse_ident program "") {
    | Ok fname =>
      switch (StringMap.get fname funcs) {
      | Some func =>
        switch (parse_args program []) {
        | Ok args =>
          let cmd = {func, args, line: CharStream.line program};
          parse_program program funcs [cmd, ...acc]
        | Error e => Error e
        }
      | None => Error ("Couldn't find command named " ^ fname)
      }
    | Error e => Error e
    }
  | Some '\n' =>
    CharStream.junk program;
    parse_program program funcs acc
  | Some '#' =>
    Parse.pop_until_newline program;
    parse_program program funcs acc
  | Some c => Error (Parse.append_char "Unexpected character " c)
  | None => Ok (Array.of_list (List.rev acc))
  };

let cmd
    (state: stateT)
    (cmd: cmdT)
    cb::(cb: stateT => err::option string => unit) =>
  cmd.func
    cmd.args
    state
    cb::(
      fun
      | Ok state => cb (inc_line state) err::None
      | Error e => cb state err::(Some e)
    );

let rec run_until_error
        (state: stateT)
        ::step=false
        cb::(cb: stateT => err::option string => unit) =>
  switch state.content.(state.currLine) {
  | input =>
    cmd
      state
      input
      cb::(
        fun state ::err =>
          switch err {
          | None =>
            if step {
              cb state err::None
            } else {
              run_until_error state step::false ::cb
            }
          | Some e => cb state err::(Some e)
          }
      )
  | exception (Invalid_argument "index out of bounds") => cb state err::None
  };
