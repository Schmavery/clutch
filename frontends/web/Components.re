type pageStateT = {
  iState: Common.stateT,
  errors: list(Common.errT),
  searchStr: string,
};
let default_program = "add 1 2 c\nshow c";
type charsT;
[@bs.get_index] external getField: (charsT, string) => string = "";
[@bs.val] external chars: charsT = "window.chars";
let getUnicode = name => getField(chars, name);
[@bs.send]
external array_filteri: (array('a), ('a, int) => bool) => array('a) =
  "filter";
let stdout_text = ref("");
module EditorButton = {
  let component = ReasonReact.statelessComponent("EditorButton");
  let make = (~func, ~name, ~color, _children) => {
    ...component,
    render: self =>
      <button
        style={
          ReactDOMRe.Style.make(
            ~backgroundColor=color,
            ~fontSize="13px",
            ~fontWeight="bold",
            ~boxShadow="none",
            ~outline="none",
            ~cursor="pointer",
            (),
          )
        }
        onClick={self.handle((_, _) => func())}>
        {ReasonReact.string(name)}
      </button>,
  };
};
module Editor = {
  type actions =
    | SubmitContentForParse(int, string)
    | NoAction;
  /* Note: Before this was the only one that actually updated the content state. */
  let getTextChangeContent = event => {
    let target = ReactEvent.Form.target(event);
    let content: string = target##value;
    let cursor: int = target##selectionStart;
    SubmitContentForParse(cursor, content);
  };
  let getMouseUpContent = event => {
    let target = ReactEvent.Mouse.target(event);
    let content: string = target##value;
    let cursor: int = target##selectionStart;
    SubmitContentForParse(cursor, content);
  };
  let getKeyUpContent = event => {
    let target = ReactEvent.Keyboard.target(event);
    let keyCode: int = ReactEvent.Keyboard.keyCode(event);
    switch (keyCode) {
    | 37
    | 38
    | 39
    | 40 =>
      let content: string = target##value;
      let cursor: int = target##selectionStart;
      SubmitContentForParse(cursor, content);
    | _ => NoAction
    };
  };
  let component = ReasonReact.reducerComponent("Editor");
  let make = (~errors, ~line, ~parse, ~run, ~reset, ~programLength, _children) => {
    ...component,
    initialState: () => default_program,
    reducer: (action, _state) =>
      switch (action) {
      | SubmitContentForParse(cursor, content) =>
        parse(Some(cursor), content);
        ReasonReact.Update(content);
      | NoAction => ReasonReact.NoUpdate
      },
    render: self => {
      let (lineColor, linePos) =
        switch (errors, line) {
        | ([], 0) => ("#fff", line - 1)
        | ([], line) => ("#dee1e8", line - 1)
        | ([{Common.line, err: _}, ..._], _) => ("#ff9393", line)
        };
      let (bgColor, resetColor) =
        switch (line) {
        | 0 => ("#fff", "#dee1e8")
        | _ => ("#f4f4f4", "#21e024")
        };
      let stepColor =
        if (programLength == line) {
          "#dee1e8";
        } else {
          "#21e024";
        };
      let backgroundPosition =
        "0px " ++ string_of_int(20 * linePos + 5) ++ "px";
      let lineGradient =
        "linear-gradient(to bottom, "
        ++ bgColor
        ++ " 0px, "
        ++ lineColor
        ++ " 0px, "
        ++ lineColor
        ++ " 22px, "
        ++ bgColor
        ++ " 22px)"
        ++ backgroundPosition;
      <div style={ReactDOMRe.Style.make(~flex="2 0 0", ())}>
        <div
          style={
            ReactDOMRe.Style.make(
              ~display="flex",
              ~flexDirection="column",
              ~height="100%",
              (),
            )
          }>
          <div
            style={
              ReactDOMRe.Style.make(
                ~display="flex",
                ~justifyContent="space-around",
                (),
              )
            }>
            <EditorButton
              func=reset
              name={"Reset " ++ getUnicode("restart")}
              color=resetColor
            />
            <EditorButton
              func={() => run(false)}
              name={"Run " ++ getUnicode("play")}
              color=stepColor
            />
            <EditorButton
              func={() => run(true)}
              name="Step"
              color=stepColor
            />
          </div>
          <textarea
            spellCheck=false
            autoComplete="off"
            style={
              ReactDOMRe.Style.make(
                ~flex="1",
                ~padding="5px",
                ~margin="5px",
                ~boxShadow="none",
                ~outline="none",
                ~lineHeight="20px",
                ~fontSize="13px",
                ~fontWeight="bold",
                ~background=lineGradient,
                ~backgroundAttachment="local",
                (),
              )
            }
            onChange={
              ev => line == 0 ? self.send(getTextChangeContent(ev)) : ()
            }
            onMouseUp={
              ev => line == 0 ? self.send(getMouseUpContent(ev)) : ()
            }
            onKeyUp={ev => line == 0 ? self.send(getKeyUpContent(ev)) : ()}
            value={self.state}
          />
        </div>
      </div>;
    },
  };
};
module Console = {
  let component = ReasonReact.statelessComponent("Console");
  let make = _children => {
    ...component,
    render: _ =>
      <div
        style={
          ReactDOMRe.Style.make(
            ~border="1px solid grey",
            ~backgroundColor="#3a3a3a",
            ~color="#e5e5e5",
            ~padding="5px",
            ~margin="5px",
            ~flex="1 1 0",
            ~overflow="hidden",
            (),
          )
        }>
        <pre> {ReasonReact.string(stdout_text^)} </pre>
      </div>,
  };
};
module Variables = {
  let component = ReasonReact.statelessComponent("Variables");
  let make = (~variables, _children) => {
    ...component,
    render: _ =>
      <div
        style={
          ReactDOMRe.Style.make(
            ~border="1px solid grey",
            ~padding="5px",
            ~margin="5px",
            ~flex="1 1 0",
            (),
          )
        }>
        {
          ReasonReact.array(
            Array.mapi(
              (varid, (k, v)) =>
                <div
                  key={string_of_int(varid)}
                  style={
                    ReactDOMRe.Style.make(
                      ~backgroundColor="#cff5f9",
                      ~border="1px solid grey",
                      ~padding="5px",
                      ~marginBottom="2px",
                      ~display="flex",
                      ~justifyContent="space-between",
                      (),
                    )
                  }>
                  <div> {ReasonReact.string(k)} </div>
                  <div>
                    {ReasonReact.string(Common.to_visualize_string(v))}
                  </div>
                </div>,
              Array.of_list(Common.StringMap.bindings(variables)),
            ),
          )
        }
      </div>,
  };
};
module ErrorList = {
  let component = ReasonReact.statelessComponent("ErrorList");
  let make = (~errors, _children) => {
    ...component,
    render: _ =>
      <div
        style={
          ReactDOMRe.Style.make(
            ~padding="5px",
            ~margin="5px",
            ~flex="1 1 0",
            ~display="flex",
            ~flexDirection="column",
            ~justifyContent="flex-end",
            ~overflow="scroll",
            (),
          )
        }>
        <div style={ReactDOMRe.Style.make(~overflow="scroll", ())}>
          {
            ReasonReact.array(
              Array.mapi(
                (errid, {Common.err}) =>
                  <div
                    key={string_of_int(errid)}
                    style={
                      ReactDOMRe.Style.make(
                        ~backgroundColor="#c41515",
                        ~border="1px solid white",
                        ~padding="5px",
                        ~color="white",
                        ~display="flex",
                        (),
                      )
                    }>
                    {ReasonReact.string(err)}
                  </div>,
                Array.of_list(errors),
              ),
            )
          }
        </div>
      </div>,
  };
};
module SearchList = {
  let component = ReasonReact.statelessComponent("ErrorList");
  let make = (~searchStr, _children) => {
    ...component,
    render: _ =>
      <div
        style={
          ReactDOMRe.Style.make(
            ~padding="5px",
            ~margin="5px",
            ~flex="1 1 0",
            ~display="flex",
            ~flexDirection="column",
            ~justifyContent="flex-end",
            ~overflow="scroll",
            (),
          )
        }>
        <div style={ReactDOMRe.Style.make(~overflow="scroll", ())}>
          {
            ReasonReact.array(
              Array.mapi(
                (id, res) =>
                  <div
                    key={string_of_int(id)}
                    style={
                      ReactDOMRe.Style.make(
                        ~backgroundColor="#f4f4f4",
                        ~border="1px solid grey",
                        ~padding="5px",
                        ~display="flex",
                        (),
                      )
                    }>
                    {ReasonReact.string(res)}
                  </div>,
                Array.of_list(
                  String.length(searchStr) > 0 ? [searchStr] : [],
                ),
              ),
            )
          }
        </div>
      </div>,
  };
};
let builtins_list =
  Builtins.[
    add,
    sub,
    mul,
    div,
    move,
    goto,
    print(s => stdout_text := stdout_text^ ++ s),
  ];
let funcs = Builtins.load_builtins_list(builtins_list, Builtins.empty);
module Page = {
  type action =
    | ISuccess(Interpret.t)
    | IErrors(Common.errT, Interpret.t)
    | UpdateSearch(string)
    | Reset;
  let parse_helper = (cursor, content) => {
    let s = CharStream.create(content);
    let res =
      Interpret.parse_program(s, funcs, ~cursor?, [], Common.StringMap.empty);
    switch (res) {
    | Common.ParseOk((cmds, labels)) =>
      ISuccess({
        variables: Common.StringMap.empty,
        content: cmds,
        currLine: 0,
        currCmd: 0,
        labels,
      })
    | ParseError(e) => IErrors(e, Interpret.empty)
    | Typing(ident) => UpdateSearch(ident)
    };
  };
  let runProgram = (self, step) => {
    let _stopProgram = ref(false);
    Interpret.run_until_error(
      self.ReasonReact.state.iState,
      ~step,
      ~stop=_stopProgram,
      ~cb=(state, ~err) =>
      switch (err) {
      | None => self.send(ISuccess(state))
      | Some(errors) => self.send(IErrors(errors, state))
      }
    );
  };
  let component = ReasonReact.reducerComponent("Page");
  let empty = {iState: Interpret.empty, errors: [], searchStr: ""};
  let make = _children => {
    ...component,
    initialState: () => {
      let s = CharStream.create(default_program);
      let res = Interpret.parse_program(s, funcs, [], Common.StringMap.empty);
      switch (res) {
      | Common.ParseOk((content, labels)) => {
          ...empty,
          iState: {
            variables: Common.StringMap.empty,
            currLine: 0,
            currCmd: 0,
            content,
            labels,
          },
        }
      | ParseError(e) => {...empty, errors: [e]}
      | Typing(ident) => {...empty, searchStr: ident}
      };
    },
    reducer: (action, state) =>
      switch (action) {
      | ISuccess(iState) => ReasonReact.Update({...empty, iState})
      | IErrors(e, iState) =>
        ReasonReact.Update({errors: [e], iState, searchStr: ""})
      | UpdateSearch(ident) =>
        ReasonReact.Update({...state, errors: [], searchStr: ident})
      | Reset =>
        stdout_text := "";
        ReasonReact.Update({
          ...empty,
          iState: {
            ...state.iState,
            currLine: 0,
            currCmd: 0,
            variables: Common.StringMap.empty,
          },
        });
      },
    render: ({state: {iState, errors}} as self) =>
      <div
        style={
          ReactDOMRe.Style.make(
            ~backgroundColor="#dee1e8",
            ~width="100%",
            ~height="100%",
            (),
          )
        }>
        <div
          style={
            ReactDOMRe.Style.make(
              ~margin="auto",
              ~display="flex",
              ~backgroundColor="white",
              ~padding="5px",
              ~border="1px solid grey",
              ~flexDirection="row",
              ~width="700px",
              ~height="70vh",
              ~position="absolute",
              ~left="0",
              ~right="0",
              ~top="0",
              ~bottom="0",
              ~justifyContent="center",
              (),
            )
          }>
          <Variables variables={iState.Common.variables} />
          <Editor
            errors
            programLength={Array.length(self.state.iState.content)}
            line={self.state.iState.currLine}
            parse={
              (cursor, content) => self.send(parse_helper(cursor, content))
            }
            run={runProgram(self)}
            reset={() => self.send(Reset)}
          />
          <div
            style={
              ReactDOMRe.Style.make(
                ~display="flex",
                ~flex="2 1 0",
                ~flexDirection="column",
                (),
              )
            }>
            <Console />
            <SearchList searchStr={self.state.searchStr} />
            <ErrorList errors />
          </div>
        </div>
      </div>,
  };
};
