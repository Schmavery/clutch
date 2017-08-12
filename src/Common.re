module StringMap = {
  include Map.Make String;
  let get key map =>
    try (Some (find key map)) {
    | _ => None
    };
  let get_default key map default =>
    try (find key map) {
    | _ => default
    };
  let update_default key map ::default update => {
    let curr_v =
      try (find key map) {
      | _ => default
      };
    add key (update curr_v) map
  };
  let union m1 m2 =>
    merge
      (
        fun _key v1 v2 =>
          switch (v1, v2) {
          | (None, Some x)
          | (Some x, _) => Some x
          | _ => None
          }
      )
      m1
      m2;
  let to_string (table: t string) =>
    fold (fun k v a => a ^ k ^ ":\t" ^ v ^ "\n") table "";
};

let split_char str ::on =>
  if (str == "") {
    []
  } else {
    let rec loop acc offset =>
      try {
        let index = String.rindex_from str offset on;
        if (index == offset) {
          loop ["", ...acc] (index - 1)
        } else {
          let token = String.sub str (index + 1) (offset - index);
          loop [token, ...acc] (index - 1)
        }
      } {
      | Not_found => [String.sub str 0 (offset + 1), ...acc]
      };
    loop [] (String.length str - 1)
  };

type result 'a 'b =
  | Ok 'a
  | Error 'b;

type valueT =
  | Num float
  | Str string;

type argT =
  | Var string
  | Val valueT;

type functionT =
  list argT => stateT => cb::(result stateT string => unit) => unit
and stateT = {
  variables: StringMap.t valueT,
  functions: StringMap.t functionT
};

let add_function (name: string) (fn: functionT) (s: stateT) :stateT => {
  ...s,
  functions: StringMap.add name fn s.functions
};

let add_variable (name: string) (value: valueT) (s: stateT) :stateT => {
  ...s,
  variables: StringMap.add name value s.variables
};

let resolve v state =>
  switch v {
  | Var v => StringMap.get_default v state.variables (Num 0.)
  | Val v => v
  };

let to_string v =>
  switch v {
  | Num f => Printf.sprintf "%g" f
  | Str s => s
  };

let to_visualize_string v =>
  switch v {
  | Num f => Printf.sprintf "%g" f
  | Str s => "\"" ^ s ^ "\""
  };
