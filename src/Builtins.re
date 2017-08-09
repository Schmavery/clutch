open Common;

let arith name op state =>
  add_function
    name
    (
      fun l state ::cb => {
        let state =
          switch l {
          | [a, b, Var c] =>
            switch (resolve a state, resolve b state) {
            | (Num a, Num b) => add_variable c (Num (op a b)) state
            /* | (Str a, Str b) => print_endline s */
            | _ =>
              print_endline "Need 2 numbers in call to add";
              state
            }
          | _ =>
            print_endline "Expected 3 args in call to add";
            state
          };
        cb state
      }
    )
    state;

let load_builtins state => {
  let state = arith "add" (+.) state;
  let state = arith "sub" (-.) state;
  let state = arith "mul" ( *. ) state;
  let state = arith "div" (/.) state;
  let state =
    add_function
      "print"
      (
        fun l state ::cb => {
          switch l {
          | [v] =>
            switch (resolve v state) {
            | Num f => Printf.printf "%g\n" f
            | Str s => print_endline s
            }
          | _ => print_endline "Expected one argument in call to print"
          };
          cb state
        }
      )
      state;
  state
};
