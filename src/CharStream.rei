type t;

let peek: t => option char;

let second: t => option char;

let junk: t => unit;

let line: t => int;

let eat_spaces: t => unit;

let clone: t => t;

let create: string => t;
