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
            | (Num a, Num b) =>
              return (Ok (add_variable c (Num (op a b)) state))
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
            return (Ok (add_variable dest srcVal state))
          }
        )
      | [_, _] => Error "Second argument to move must be a variable name"
      | _ => Error "Expected 2 args in call to move"
    )
    funcs;

let load_builtins_list
    (lst: list (StringMap.t functionT => StringMap.t functionT))
    (state: StringMap.t functionT)
    :StringMap.t functionT =>
  List.fold_left (fun acc v => v acc) state lst;
