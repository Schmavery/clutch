open Common;

let empty = {
  funcs: StringMap.empty,
  docs: [
    {
      name: "label",
      aliases: [],
      signature: "name",
      description: "You can jump to this line by using 'goto' and the name after the 'label'.",
    },
  ],
};

let add_function =
    (name, ~signature, ~aliases=[], ~desc, func, {funcs, docs}: funcsT) => {
  funcs: StringMap.add(name, func, funcs),
  docs: [{name, aliases, signature, description: desc}, ...docs],
};

let arith = (name, action, op, funcs: funcsT) =>
  add_function(
    name,
    ~signature="number number variable",
    ~desc=
      action ++ " first two numbers and puts the results in the last variable.",
    fun
    | [_, _, Val(v)] =>
      Error(
        "Last input to "
        ++ name
        ++ " was "
        ++ to_visualize_string(v)
        ++ ", this should be a variable instead",
      )
    | [Val(Str(s)), _, _]
    | [_, Val(Str(s)), _] =>
      Error(
        "Input to "
        ++ name
        ++ " was \""
        ++ s
        ++ "\", this should be a number instead",
      )
    | [a, b, Var(c)] =>
      Ok(
        (
          (state, ~cb as return) =>
            switch (resolve(a, state), resolve(b, state)) {
            | (Num(a), Num(b)) =>
              return(add_variable(c, Num(op(a, b)), state))
            | _ => return(Error("Need 2 numbers in call to add"))
            }
        ),
      )
    | _ => Error("Expected 3 args in call to add"),
    funcs,
  );

let add = funcs => arith("add", "Adds", (+.), funcs);

let sub = funcs => arith("sub", "Subtracts", (-.), funcs);

let mul = funcs => arith("mul", "Multiplies", ( *. ), funcs);

let div = funcs => arith("div", "Divides", (/.), funcs);

let print = (stdout: string => unit, funcs) => {
  let funcs =
    add_function(
      "show",
      ~signature="anything",
      ~desc="Shows the value to the screen",
      fun
      | [v] =>
        Ok(
          (
            (state, ~cb as return) =>
              return(
                Ok(
                  {
                    stdout(to_string(resolve(v, state)));
                    state;
                  },
                ),
              )
          ),
        )
      | _ => Error("Only expected one argument in call to show"),
      funcs,
    );
  add_function(
    "line",
    ~signature="",
    ~desc="Makes the cursor move down one line",
    fun
    | [] =>
      Ok(
        (
          (state, ~cb as return) =>
            return(
              Ok(
                {
                  stdout("\n");
                  state;
                },
              ),
            )
        ),
      )
    | _ => Error("Expected no arguments in call to line"),
    funcs,
  );
};

let move = funcs =>
  add_function(
    "move",
    ~signature="anything variable",
    ~desc="Moves the first value into the second variable",
    fun
    | [src, Var(dest)] =>
      Ok(
        (
          (state, ~cb as return) => {
            let srcVal = resolve(src, state);
            return(add_variable(dest, srcVal, state));
          }
        ),
      )
    | [_, _] => Error("Second argument to move must be a variable name")
    | _ => Error("Expected 2 args in call to move"),
    funcs,
  );

let goto = (funcs: funcsT) => {
  let funcs =
    add_function(
      "goto",
      ~signature="label",
      ~aliases=["jump"],
      ~desc="Makes the next instruction be the specified label",
      fun
      | [Var(name)] =>
        Ok(
          (
            (state, ~cb as return) =>
              switch (StringMap.find(name, state.labels)) {
              | (currCmd, currLine) =>
                return(Ok({...state, currCmd: currCmd - 1, currLine}))
              | exception Not_found =>
                return(
                  Error(
                    Printf.sprintf(
                      "You tried to goto '%s', but you didn't mark a label for it to jump to.",
                      name,
                    ),
                  ),
                )
              }
          ): innerFuncT,
        )
      | [Val(v)] =>
        Error(
          Printf.sprintf(
            "Input to goto must be a label, you gave a %s instead",
            to_type(v),
          ),
        )
      | _ => Error("goto expects one variable as input"),
      funcs,
    );
  funcs;
};

let load_builtins_list = (lst: list(funcsT => funcsT), funcs: funcsT): funcsT =>
  List.fold_left((acc, v) => v(acc), funcs, lst);
