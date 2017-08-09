open Common;

let append_char (s: string) (c: char) :string => s ^ String.make 1 c;

let pop_stream s => {
  Stream.junk s;
  s
};

let rec parse_string
        (stream: Stream.t char)
        (acc: string)
        :result string string =>
  switch (Stream.peek stream) {
  | Some '\\' =>
    Stream.junk stream;
    switch (Stream.peek stream) {
    | Some 'n' => parse_string (pop_stream stream) (append_char acc '\n')
    | Some 't' => parse_string (pop_stream stream) (append_char acc '\t')
    | Some '"' => parse_string (pop_stream stream) (append_char acc '"')
    | Some '\'' => parse_string (pop_stream stream) (append_char acc '\'')
    | Some '\\' => parse_string (pop_stream stream) (append_char acc '\\')
    | Some c => Error ("Invalid escape sequence \\" ^ append_char "" c ^ ".")
    | None => Error "Unterminated string."
    }
  | Some '"' => Ok acc
  | Some c => parse_string (pop_stream stream) (append_char acc c)
  | None => Error "Unterminated string."
  };

let rec parse_ident (stream: Stream.t char) (acc: string) :result string string =>
  switch (Stream.peek stream) {
  | None
  | Some '\t'
  | Some '\n'
  | Some ' ' => Ok acc
  | Some c => parse_ident (pop_stream stream) (append_char acc c)
  };

let rec parse_num (stream: Stream.t char) (acc: string) :result float string =>
  switch (Stream.peek stream) {
  | Some ('.' as c)
  | Some ('0'..'9' as c) => parse_num (pop_stream stream) (append_char acc c)
  | _ =>
    let num =
      try (Some (float_of_string acc)) {
      | _ => None
      };
    switch num {
    | Some f => Ok f
    | None => Error ("Could not parse number [" ^ acc ^ "].")
    }
  };

let rec pop_until_newline (stream: Stream.t char) =>
  switch (Stream.peek stream) {
  | Some '\n' => Stream.junk stream
  | Some _ => pop_until_newline (pop_stream stream)
  | None => ()
  };

