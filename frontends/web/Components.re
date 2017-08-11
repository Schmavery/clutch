let makeStyle = ReactDOMRe.Style.make;

type charsT;

external getField : charsT => string => string = "" [@@bs.get_index];

external chars : charsT = "window.chars" [@@bs.val];

let getUnicode name => getField chars name;

let stdout_text = ref "";

module RunButton = {
  let component = ReasonReact.statelessComponent "RunButton";
  let make ::runFunc _children => {
    ...component,
    render: fun self =>
      <button
        style=(
          makeStyle
            minHeight::"30px" width::"100px" backgroundColor::"#21e024" ()
        )
        onClick=(self.handle (fun _ _ => runFunc ()))>
        (ReasonReact.stringToElement "Run")
      </button>
  };
};

module Editor = {
  let component = ReasonReact.statefulComponent "FileComponent";
  let textChange event _state =>
    ReasonReact.Update (
                         ReactDOMRe.domElementToObj (
                           ReactEventRe.Form.target event
                         )
                       )##value;
  let make ::eval _children => {
    ...component,
    initialState: fun () => "add 1 2",
    render: fun self =>
      <div style=(makeStyle flex::"1 1 0" ())>
        <div
          style=(
            makeStyle
              display::"flex"
              flexDirection::"column"
              height::"100%"
              /* flex::"1 1 0" */
              ()
          )>
          <div
            style=(makeStyle display::"flex" justifyContent::"space-around" ())>
            <RunButton runFunc=(eval self.state) />
          </div>
          /* <button> Step <button /> */
          /* <button> Step <button /> */
          <textarea
            style=(makeStyle flex::"1" padding::"5px" margin::"5px" ())
            onChange=(self.update textChange)
            value=self.state
          />
        </div>
      </div>
  };
};

module Console = {
  let component = ReasonReact.statelessComponent "Console";
  let make _children => {
    ...component,
    render: fun _ => {
      print_endline !stdout_text;
      <div
        style=(
          makeStyle
            border::"1px solid grey"
            padding::"5px"
            margin::"5px"
            flex::"1 1 0"
            ()
        )>
        <pre> (ReasonReact.stringToElement !stdout_text) </pre>
      </div>
    }
  };
};

module Variables = {
  let component = ReasonReact.statelessComponent "Variables";
  let make ::variables _children => {
    ...component,
    render: fun _ =>
      <div
        style=(
          makeStyle
            border::"1px solid grey"
            padding::"5px"
            margin::"5px"
            flex::"1 1 0"
            ()
        )>
        (ReasonReact.stringToElement "Variables:")
      </div>
  };
};

module ErrorList = {
  let component = ReasonReact.statelessComponent "ErrorList";
  let make ::errors _children => {
    ...component,
    render: fun _ =>
      <div
        style=(
          makeStyle
            padding::"5px"
            margin::"5px"
            flex::"1 1 0"
            display::"flex"
            flexDirection::"column"
            justifyContent::"flex-end"
            overflow::"scroll"
            ()
        )>
        <div style=(makeStyle overflow::"scroll" ())>
          (
            ReasonReact.arrayToElement (
              Array.mapi
                (
                  fun i s =>
                    <div
                      key=(string_of_int i)
                      style=(
                        makeStyle
                          backgroundColor::"#c41515"
                          border::"1px solid white"
                          padding::"5px"
                          color::"white"
                          ()
                      )>
                      (ReasonReact.stringToElement s)
                    </div>
                )
                errors
            )
          )
        </div>
      </div>
  };
};

module Page = {
  type pageStateT = {
    interpretState: Common.stateT,
    errors: array string
  };
  let component = ReasonReact.statefulComponent "Page";
  let runProgram self (content: string) () =>
    Interpret.cmd
      self.ReasonReact.state.interpretState
      content
      cb::(
        fun res =>
          self.update
            (
              fun () self =>
                ReasonReact.Update (
                  switch res {
                  | Ok new_istate => {
                      ...self.state,
                      interpretState: new_istate
                    }
                  | Error e => {
                      ...self.state,
                      errors: Array.append [|e|] self.state.errors
                    }
                  }
                )
            )
            ()
      );
  let builtins_list =
    Builtins.[
      add,
      sub,
      mul,
      div,
      print (fun s => stdout_text := !stdout_text ^ s)
    ];
  let make _children => {
    ...component,
    initialState: fun () => {
      interpretState: Builtins.load_builtins_list builtins_list Interpret.empty,
      errors: [||]
    },
    render: fun ({state: {interpretState, errors}} as self) =>
      <div
        style=(
          makeStyle
            display::"flex"
            flexDirection::"row"
            /* flexGrow::"1" */
            width::"70vw"
            height::"70vh"
            /* overflow::"auto" */
            /* justifyContent::"space-between" */
            /* flex::"1 1 auto" */
            /* alignItems::"stretch" */
            /* position::"relative" */
            ()
        )>
        <Variables variables=interpretState.Common.variables />
        <Editor eval=(runProgram self) />
        <div
          style=(
            makeStyle display::"flex" flex::"1 1 0" flexDirection::"column" ()
          )>
          <Console />
          <ErrorList errors />
        </div>
      </div>
  };
};
