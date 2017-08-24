open Common;

let arith name op (funcs: StringMap.t functionT) =>
  StringMap.add
    name
    (
      fun
      | [_, _, Val v] =>
        Error (
          "Last input to " ^
          name ^
          " was " ^
          to_visualize_string v ^ ", this should be a variable instead"
        )
      | [Val (Str s), _, _]
      | [_, Val (Str s), _] =>
        Error (
          "Input to " ^
          name ^ " was \"" ^ s ^ "\", this should be a number instead"
        )
      | [a, b, Var c] =>
        Ok (
          fun state cb::return =>
            switch (resolve a state, resolve b state) {
            | (Num a, Num b) => return (add_variable c (Num (op a b)) state)
            | _ => return (Error "Need 2 numbers in call to add")
            }
        )
      | _ => Error "Expected 3 args in call to add"
    )
    funcs;

let add funcs => arith "add" (+.) funcs;

let sub funcs => arith "sub" (-.) funcs;

let mul funcs => arith "mul" ( *. ) funcs;

let div funcs => arith "div" (/.) funcs;

let print (stdout: string => unit) funcs => {
  let funcs =
    StringMap.add
      "print"
      (
        fun
        | [v] =>
          Ok (
            fun state cb::return =>
              return (
                Ok {
                  stdout (to_string (resolve v state));
                  state
                }
              )
          )
        | _ => Error "Only expected one argument in call to print"
      )
      funcs;
  StringMap.add
    "line"
    (
      fun
      | [] =>
        Ok (
          fun state cb::return =>
            return (
              Ok {
                stdout "\n";
                state
              }
            )
        )
      | _ => Error "Expected no arguments in call to line"
    )
    funcs
};

let move funcs =>
  StringMap.add
    "move"
    (
      fun
      | [src, Var dest] =>
        Ok (
          fun state cb::return => {
            let srcVal = resolve src state;
            return (add_variable dest srcVal state)
          }
        )
      | [_, _] => Error "Second argument to move must be a variable name"
      | _ => Error "Expected 2 args in call to move"
    )
    funcs;

let goto (funcs: StringMap.t functionT) => {
  let funcs =
    StringMap.add
      "goto"
      (
        fun
        | [Var name] =>
          Ok (
            (
              fun state cb::return =>
                switch (StringMap.find name state.labels) {
                | (currCmd, currLine) =>
                  return (Ok {...state, currCmd: currCmd - 1, currLine})
                | exception Not_found =>
                  return (
                    Error (
                      Printf.sprintf
                        "You tried to goto '%s', but you didn't mark a label for it to jump to."
                        name
                    )
                  )
                }
            ): innerFuncT
          )
        | [Val v] =>
          Error (
            Printf.sprintf
              "Input to goto must be a label, you gave a %s instead"
              (to_type v)
          )
        | _ => Error "goto expects one variable as input"
      )
      funcs;
  funcs
};

let load_builtins_list
    (lst: list (StringMap.t functionT => StringMap.t functionT))
    (funcs: StringMap.t functionT)
    :StringMap.t functionT =>
  List.fold_left (fun acc v => v acc) funcs lst;
