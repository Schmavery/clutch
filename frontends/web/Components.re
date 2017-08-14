type pageStateT = {
  iState: Common.stateT,
  errors: array string
};

type charsT;

external getField : charsT => string => string = "" [@@bs.get_index];

external chars : charsT = "window.chars" [@@bs.val];

external array_filteri : array 'a => ('a => int => bool) => array 'a =
  "filter" [@@bs.send];

let getUnicode name => getField chars name;

let stdout_text = ref "";

module EditorButton = {
  let component = ReasonReact.statelessComponent "EditorButton";
  let make ::func ::name _children => {
    ...component,
    render: fun self =>
      <button
        style=(
          ReactDOMRe.Style.make
            backgroundColor::"#21e024"
            fontSize::"13px"
            fontWeight::"bold"
            boxShadow::"none"
            outline::"none"
            cursor::"pointer"
            ()
        )
        onClick=(self.handle (fun _ _ => func ()))>
        (ReasonReact.stringToElement name)
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
  let make ::errors ::line ::step ::run ::reset _children => {
    ...component,
    initialState: fun () => "add 1 2 c\nprint c",
    render: fun self => {
      let lineColor =
        switch errors {
        | [||] => "#dee1e8"
        | _ => "#ff9393"
        };
      let backgroundPosition = "0px " ^ string_of_int (20 * line + 5) ^ "px";
      let lineGradient =
        "linear-gradient(to bottom, #fff 0px, " ^
        lineColor ^ " 0px, " ^ lineColor ^ " 22px, #fff 22px)" ^ backgroundPosition;
      <div style=(ReactDOMRe.Style.make flex::"2 0 0" ())>
        <div
          style=(
            ReactDOMRe.Style.make
              display::"flex" flexDirection::"column" height::"100%" ()
          )>
          <div
            style=(
              ReactDOMRe.Style.make
                display::"flex" justifyContent::"space-around" ()
            )>
            <EditorButton func=reset name=("Reset " ^ getUnicode "restart") />
            <EditorButton
              func=(run self.state)
              name=("Run " ^ getUnicode "play")
            />
            <EditorButton func=(step self.state) name="Step" />
          </div>
          <textarea
            spellCheck=Js.false_
            autoComplete="off"
            style=(
              ReactDOMRe.Style.make
                flex::"1"
                padding::"5px"
                margin::"5px"
                boxShadow::"none"
                outline::"none"
                lineHeight::"20px"
                fontSize::"13px"
                fontWeight::"bold"
                background::lineGradient
                backgroundAttachment::"local"
                /* ::backgroundPosition */
                ()
            )
            onChange=(self.update textChange)
            value=self.state
          />
        </div>
      </div>
    }
  };
};

module Console = {
  let component = ReasonReact.statelessComponent "Console";
  let make _children => {
    ...component,
    render: fun _ =>
      <div
        style=(
          ReactDOMRe.Style.make
            border::"1px solid grey"
            backgroundColor::"#3a3a3a"
            color::"#e5e5e5"
            padding::"5px"
            margin::"5px"
            flex::"1 1 0"
            overflow::"hidden"
            ()
        )>
        <pre> (ReasonReact.stringToElement !stdout_text) </pre>
      </div>
  };
};

module Variables = {
  let component = ReasonReact.statelessComponent "Variables";
  let make ::variables _children => {
    ...component,
    render: fun _ =>
      <div
        style=(
          ReactDOMRe.Style.make
            border::"1px solid grey"
            padding::"5px"
            margin::"5px"
            flex::"1 1 0"
            ()
        )>
        (
          ReasonReact.arrayToElement (
            Array.mapi
              (
                fun varid (k, v) =>
                  <div
                    key=(string_of_int varid)
                    style=(
                      ReactDOMRe.Style.make
                        backgroundColor::"#cff5f9"
                        border::"1px solid grey"
                        padding::"5px"
                        marginBottom::"2px"
                        display::"flex"
                        justifyContent::"space-between"
                        ()
                    )>
                    <div> (ReasonReact.stringToElement k) </div>
                    <div>
                      (
                        ReasonReact.stringToElement (
                          Common.to_visualize_string v
                        )
                      )
                    </div>
                  </div>
              )
              (Array.of_list (Common.StringMap.bindings variables))
          )
        )
      </div>
  };
};

module ErrorList = {
  let removeError errid _ self => {
    let errors =
      array_filteri self.ReasonReact.state.errors (fun _ i => i != errid);
    ReasonReact.Update {...self.ReasonReact.state, errors}
  };
  let component = ReasonReact.statelessComponent "ErrorList";
  let make ::errors _children => {
    ...component,
    render: fun _ =>
      <div
        style=(
          ReactDOMRe.Style.make
            padding::"5px"
            margin::"5px"
            flex::"1 1 0"
            display::"flex"
            flexDirection::"column"
            justifyContent::"flex-end"
            overflow::"scroll"
            ()
        )>
        <div style=(ReactDOMRe.Style.make overflow::"scroll" ())>
          (
            ReasonReact.arrayToElement (
              Array.mapi
                (
                  fun errid s =>
                    <div
                      key=(string_of_int errid)
                      style=(
                        ReactDOMRe.Style.make
                          backgroundColor::"#c41515"
                          border::"1px solid white"
                          padding::"5px"
                          color::"white"
                          display::"flex"
                          /* justifyContent::"space-between" */
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

let builtins_list =
  Builtins.[
    add,
    sub,
    mul,
    div,
    move,
    print (fun s => stdout_text := !stdout_text ^ s)
  ];

let funcs = Builtins.load_builtins_list builtins_list Common.StringMap.empty;

let rec drop_some l n =>
  switch (l, n) {
  | (l, 0) => l
  | ([_, ...tl], n) => drop_some tl (n - 1)
  | ([], _) => []
  };

let runCompleteProgram self (content: string) () => {
  /* TODO: Move this into the interpreter */
  let lines = Common.split_char content on::'\n';
  let lines = drop_some lines self.ReasonReact.state.iState.currLine;
  Interpret.run_until_error
    self.ReasonReact.state.iState
    funcs
    lines
    cb::(
      fun state ::err =>
        self.update
          (
            fun () self =>
              ReasonReact.Update (
                switch err {
                | None => {...self.state, iState: state}
                | Some e => {
                    iState: state,
                    errors: Array.append [|e|] self.state.errors
                  }
                }
              )
          )
          ()
    )
};

let stepProgram self (content: string) () => {
  /* TODO: here too */
  let lines = Common.split_char content on::'\n';
  let lines = drop_some lines self.ReasonReact.state.iState.currLine;
  switch lines {
  | [line, ..._] =>
    Interpret.cmd
      self.ReasonReact.state.iState
      funcs
      line
      cb::(
        fun iState ::err =>
          self.update
            (
              fun () self =>
                ReasonReact.Update (
                  switch err {
                  | None => {iState, errors: [||]}
                  | Some e => {
                      ...self.state,
                      errors: Array.append [|e|] self.state.errors
                    }
                  }
                )
            )
            ()
      )
  | [] => ()
  }
};

let resetProgram self () => {
  stdout_text := "";
  self.ReasonReact.update
    (
      fun () _self => ReasonReact.Update {iState: Interpret.empty, errors: [||]}
    )
    ()
};

module Page = {
  let component = ReasonReact.statefulComponent "Page";
  let make _children => {
    ...component,
    initialState: fun () => {iState: Interpret.empty, errors: [||]},
    render: fun ({state: {iState, errors}} as self) =>
      <div
        style=(
          ReactDOMRe.Style.make
            backgroundColor::"#dee1e8" width::"100%" height::"100%" ()
        )>
        <div
          style=(
            ReactDOMRe.Style.make
              margin::"auto"
              display::"flex"
              backgroundColor::"white"
              padding::"5px"
              border::"1px solid grey"
              flexDirection::"row"
              width::"700px"
              height::"70vh"
              position::"absolute"
              left::"0"
              right::"0"
              top::"0"
              bottom::"0"
              justifyContent::"center"
              ()
          )>
          <Variables variables=iState.Common.variables />
          <Editor
            errors
            line=self.state.iState.currLine
            step=(stepProgram self)
            run=(runCompleteProgram self)
            reset=(resetProgram self)
          />
          <div
            style=(
              ReactDOMRe.Style.make
                display::"flex" flex::"2 1 0" flexDirection::"column" ()
            )>
            <Console />
            <ErrorList errors />
          </div>
        </div>
      </div>
  };
};
