open Common;

let append_char = (s: string, c: char): string => s ++ String.make(1, c);

let pop_stream = s => {
  CharStream.junk(s);
  s;
};

let rec parse_string =
        (stream: CharStream.t, acc: string): result(string, string) =>
  switch (CharStream.peek(stream)) {
  | Some('\\') =>
    CharStream.junk(stream);
    switch (CharStream.peek(stream)) {
    | Some('n') => parse_string(pop_stream(stream), append_char(acc, '\n'))
    | Some('t') => parse_string(pop_stream(stream), append_char(acc, '\t'))
    | Some('"') => parse_string(pop_stream(stream), append_char(acc, '"'))
    | Some('\'') => parse_string(pop_stream(stream), append_char(acc, '\''))
    | Some('\\') => parse_string(pop_stream(stream), append_char(acc, '\\'))
    | Some(c) => Error(Printf.sprintf("Invalid escape sequence \\%c.", c))
    | None => Error("Unterminated string.")
    };
  | Some('"') =>
    CharStream.junk(stream);
    Ok(acc);
  | Some('\n') =>
    Error(
      "A string needs to be all on one line. Did you forget a quote at the end?",
    )
  | Some(c) => parse_string(pop_stream(stream), append_char(acc, c))
  | None => Error(Printf.sprintf("Did you miss a \" the end of '%s'?", acc))
  };

let rec parse_ident =
        (stream: CharStream.t, acc: string): result(string, string) =>
  switch (CharStream.peek(stream)) {
  | Some('a'..'z' as c)
  | Some('A'..'Z' as c)
  | Some('0'..'9' as c) =>
    parse_ident(pop_stream(stream), append_char(acc, c))
  | None
  | Some('\t')
  | Some('\n')
  | Some(' ') => Ok(acc)
  | Some('"') =>
    switch (CharStream.second(stream)) {
    | None
    | Some('\t')
    | Some('\n')
    | Some(' ') =>
      Error(
        Printf.sprintf(
          "You have a \" at the end of the variable named '%s'. Did you forget to put a \" somewhere?",
          acc,
        ),
      )
    | _ =>
      Error(
        Printf.sprintf(
          "Unexpected \" after variable named '%s'. Remember you need to put spaces between things.",
          acc,
        ),
      )
    }
  | Some(c) =>
    Error(
      Printf.sprintf("Unexpected letter '%c' after variable '%s'.", c, acc),
    )
  };

let rec parse_num =
        (stream: CharStream.t, acc: string): result(float, string) =>
  switch (CharStream.peek(stream)) {
  | Some('.' as c)
  | Some('0'..'9' as c) =>
    parse_num(pop_stream(stream), append_char(acc, c))
  | _ =>
    let num =
      try (Some(float_of_string(acc))) {
      | _ => None
      };
    switch (num) {
    | Some(f) => Ok(f)
    | None => Error("Could not parse number [" ++ acc ++ "].")
    };
  };

let rec pop_until_newline = (stream: CharStream.t) =>
  switch (CharStream.peek(stream)) {
  | Some('\n') => CharStream.junk(stream)
  | Some(_) => pop_until_newline(pop_stream(stream))
  | None => ()
  };
