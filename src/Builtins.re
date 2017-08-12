open Common;

let arith name op state =>
  add_function
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
    state;

let add state => arith "add" (+.) state;

let sub state => arith "sub" (-.) state;

let mul state => arith "mul" ( *. ) state;

let div state => arith "div" (/.) state;

let print (stdout: string => unit) state =>
  add_function
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
    state;

let line (stdout: string => unit) state =>
  add_function
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
    state;

let move state =>
  add_function
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
    state;

let load_builtins_list (lst: list (stateT => stateT)) (state: stateT) :stateT =>
  List.fold_left (fun acc v => v acc) state lst;
