open Common;

let arith name op funcs =>
  StringMap.add
    name
    (
      fun l state cb::return =>
        switch l {
        | [a, b, Var c] =>
          switch (resolve a state, resolve b state) {
          | (Num a, Num b) => return (Ok (add_variable c (Num (op a b)) state))
          | _ => return (Error "Need 2 numbers in call to add")
          }
        | _ => return (Error "Expected 3 args in call to add")
        }
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
        fun l state cb::return =>
          switch l {
          | [v] =>
            return (
              Ok {
                stdout (to_string (resolve v state));
                state
              }
            )
          | _ => return (Error "Expected one argument in call to print")
          }
      )
      funcs;
  StringMap.add
    "line"
    (
      fun l state cb::return =>
        switch l {
        | [] =>
          return (
            Ok {
              stdout "\n";
              state
            }
          )
        | _ => return (Error "Expected arguments in call to line")
        }
    )
    funcs
};

let move funcs =>
  StringMap.add
    "move"
    (
      fun l state cb::return =>
        switch l {
        | [src, Var dest] =>
          switch (resolve src state) {
          | srcVal => return (Ok (add_variable dest srcVal state))
          }
        | [_, _] => return (Error "Second argument must be a variable name")
        | _ => return (Error "Expected 2 args in call to move")
        }
    )
    funcs;

let load_builtins_list
    (lst: list (StringMap.t functionT => StringMap.t functionT))
    (state: StringMap.t functionT)
    :StringMap.t functionT =>
  List.fold_left (fun acc v => v acc) state lst;
