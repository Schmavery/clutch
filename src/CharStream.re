type trec = {
  chars: list char,
  lineNum: int,
  chNum: int
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
  | {chars: ['\n', ...lst], lineNum, chNum} =>
    stream := {chars: lst, lineNum: lineNum + 1, chNum: chNum + 1}
  | {chars: [_, ...lst], chNum} =>
    stream := {...!stream, chars: lst, chNum: chNum + 1}
  };

let line (stream: t) :int => (!stream).lineNum;

let ch (stream: t) :int => (!stream).chNum;

let rec eat_spaces (stream: t) :unit =>
  switch !stream {
  | {chars: [' ', ...lst], chNum} =>
    stream := {...!stream, chars: lst, chNum: chNum + 1};
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
  ref {chars: explode (String.length s - 1) [], lineNum: 0, chNum: 0}
};
