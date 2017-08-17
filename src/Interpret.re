open Common;

let error program err => Error {err, line: CharStream.line program};

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
        (funcs: StringMap.t functionT)
        (acc: list cmdT)
        :result (array cmdT) errT =>
  switch (CharStream.peek program) {
  | Some 'a'..'z'
  | Some 'A'..'Z' =>
    switch (Parse.parse_ident program "") {
    | Ok fname =>
      switch (StringMap.get fname funcs) {
      | Some func =>
        switch (parse_args program []) {
        | Ok args =>
          switch (func args) {
          | Ok innerFunc =>
            let cmd = {func: innerFunc, line: CharStream.line program};
            parse_program program funcs [cmd, ...acc]
          | Error e => error program e
          }
        | Error e => error program e
        }
      | None => error program ("Couldn't find command named " ^ fname)
      }
    | Error e => error program e
    }
  | Some '\n' =>
    CharStream.junk program;
    parse_program program funcs acc
  | Some '#' =>
    Parse.pop_until_newline program;
    parse_program program funcs acc
  | Some c =>
    Error {
      err: Parse.append_char "Unexpected character " c,
      line: CharStream.line program
    }
  | None => Ok (Array.of_list (List.rev acc))
  };

let cmd
    (state: stateT)
    (cmd: cmdT)
    cb::(cb: stateT => err::option errT => unit) =>
  cmd.func
    state
    cb::(
      fun
      | Ok state => cb (inc_line state) err::None
      | Error e => cb state err::(Some {err: e, line: cmd.line})
    );

let rec run_until_error
        (state: stateT)
        ::step=false
        ::stop=?
        cb::(cb: stateT => err::option errT => unit) =>
  switch state.content.(state.currLine) {
  | input =>
    cmd
      state
      input
      cb::(
        fun state ::err =>
          switch err {
          | None =>
            switch (step, stop) {
            | (true, _) => cb state err::None
            | (false, Some {contents: true})
            | (false, None)
            | (false, Some {contents: false}) =>
              ignore @@
              Js.Global.setTimeout
                (fun () => run_until_error state ::step ::?stop ::cb) 0
            }
          | Some e => cb state err::(Some e)
          }
      )
  | exception (Invalid_argument "index out of bounds") => cb state err::None
  };
