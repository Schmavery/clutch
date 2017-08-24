open Common;

let error program err => Error {err, line: CharStream.line program};

let parseError prevCh (program: CharStream.t) (cursor: option int) err =>
  switch cursor {
  | Some ch when CharStream.ch program + 1 > ch && prevCh < ch => Typing
  | _ => ParseError {err, line: CharStream.line program}
  };

let empty = {
  variables: StringMap.empty,
  labels: StringMap.empty,
  currLine: 0,
  currCmd: 0,
  content: [||]
};

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

let rec parse_args stream acc cursor :parseResult (list argT) errT => {
  let currCh = CharStream.ch stream;
  switch (CharStream.peek stream) {
  | Some ' ' =>
    CharStream.eat_spaces stream;
    parse_args stream acc cursor
  | None
  | Some '\n' => ParseOk (List.rev acc)
  | Some _ =>
    switch (parse_arg stream) {
    | Ok a => parse_args stream [a, ...acc] cursor
    | Error e => parseError currCh stream cursor e
    }
  }
};

let rec parse_program
        (program: CharStream.t)
        (funcs: StringMap.t functionT)
        ::cursor=?
        (cmds: list cmdT)
        (labels: StringMap.t (int, int))
        :parseResult (array cmdT, StringMap.t (int, int)) errT => {
  let prevCh = CharStream.ch program;
  switch (CharStream.peek program) {
  | Some 'a'..'z'
  | Some 'A'..'Z' =>
    switch (Parse.parse_ident program "", parse_args program [] cursor) {
    | (Ok "label", ParseOk args) =>
      handleLabel program args prevCh funcs cursor cmds labels
    | (Ok fname, ParseOk args) =>
      switch (StringMap.get fname funcs) {
      | Some func =>
        switch (func args) {
        | Ok innerFunc =>
          let cmd = {
            name: fname,
            func: innerFunc,
            line: CharStream.line program
          };
          parse_program program funcs ::?cursor [cmd, ...cmds] labels
        | Error err => parseError prevCh program cursor err
        }
      | None =>
        parseError
          prevCh program cursor ("Couldn't find command named '" ^ fname ^ "'")
      }
    | (Ok _, ParseError e) => ParseError e
    | (Ok _, Typing) => Typing
    | (Error e, _) => parseError prevCh program cursor e
    }
  | Some '\n' =>
    CharStream.junk program;
    parse_program program funcs ::?cursor cmds labels
  | Some '#' =>
    Parse.pop_until_newline program;
    parse_program program funcs ::?cursor cmds labels
  | Some c =>
    parseError
      prevCh program cursor (Parse.append_char "Unexpected character " c)
  | None => ParseOk (Array.of_list (List.rev cmds), labels)
  }
}
and handleLabel program args prevCh funcs cursor cmds labels =>
  switch args {
  | [Var name] =>
    switch (StringMap.find name labels) {
    | (_cmd, line) =>
      parseError
        prevCh
        program
        cursor
        (
          Printf.sprintf
            "There is already a label with the name '%s' at line %i." name line
        )
    | exception Not_found =>
      parse_program
        program
        funcs
        ::?cursor
        cmds
        (StringMap.add name (List.length cmds, CharStream.line program) labels)
    }
  | [Val v] =>
    parseError
      prevCh
      program
      cursor
      (
        Printf.sprintf
          "Input to label must be a variable, you gave a %s instead"
          (to_type v)
      )
  | _ => parseError prevCh program cursor "label expects one variable as input"
  };

let cmd
    (state: stateT)
    (cmd: cmdT)
    cb::(cb: stateT => err::option errT => unit) =>
  cmd.func
    state
    cb::(
      fun
      | Ok state =>
        cb
          {...state, currCmd: state.currCmd + 1, currLine: cmd.line + 1}
          err::None
      | Error e => cb state err::(Some {err: e, line: cmd.line})
    );

let rec run_until_error
        (state: stateT)
        ::step=false
        ::stop=?
        cb::(cb: stateT => err::option errT => unit) =>
  switch state.content.(state.currCmd) {
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
