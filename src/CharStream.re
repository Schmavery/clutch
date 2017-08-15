type trec = {
  chars: list char,
  lineNum: int
};

type t = ref trec;

let peek (stream: t) :option char =>
  switch !stream {
  | {chars: []} => None
  | {chars: [c, ..._]} => Some c
  };

let second (stream: t) :option char =>
  switch !stream {
  | {chars: []}
  | {chars: [_]} => None
  | {chars: [_, c, ..._]} => Some c
  };

let junk (stream: t) :unit =>
  switch !stream {
  | {chars: []} => raise (Failure "Empty CharStream")
  | {chars: ['\n', ...lst], lineNum} =>
    stream := {chars: lst, lineNum: lineNum + 1}
  | {chars: [_, ...lst]} => stream := {...!stream, chars: lst}
  };

let line (stream: t) :int => (!stream).lineNum;

let rec eat_spaces (stream: t) :unit =>
  switch !stream {
  | {chars: [' ', ...lst]} =>
    stream := {...!stream, chars: lst};
    eat_spaces stream
  | _ => ()
  };

let clone (stream: t) :t => ref !stream;

let create (s: string) :t => {
  let rec explode (i: int) (acc: list char) :list char =>
    if (i < 0) {
      acc
    } else {
      explode (i - 1) [s.[i], ...acc]
    };
  ref {chars: explode (String.length s - 1) [], lineNum: 0}
};
