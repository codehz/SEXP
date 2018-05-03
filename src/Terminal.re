open ReasonReact;

type line = {
  data: SExp.t,
  source: string,
  time: string,
};

type prompt = {
  indicator: string,
  handler: SExp.t => unit,
};

type state = {
  buffer: list(line),
  mods: Eval.smod,
  minibuffer: SExp.t,
};

type action =
  | ClearBuffer
  | AppendBuffer(SExp.t, string)
  | Prompt(string, SExp.t => unit)
  | DefineModule(string, Eval.definition)
  | Execute
  | Update(SExp.t);

let component = reducerComponent("Terminal");

module EvelInstance =
  Eval.Make(
    {
      type t = self(state, noRetainedProps, action);
      let write = (self, text) => AppendBuffer(text, "output") |> self.send;
      let prompt = (self, prompt, callback) =>
        Prompt(prompt, callback) |> self.send;
      let defineModule = (self, name, body) =>
        DefineModule(name, body) |> self.send;
      let loadModule = (self, name) =>
        switch (Eval.SModMap.find(name, self.state.mods)) {
        | modu => Some(modu)
        | exception Not_found => None
        };
    },
  );

module Label = {
  let component = statelessComponent("Label");
  let make = (~value, ~clazz, _children) => {
    ...component,
    render: _self =>
      <div className=(clazz |> String.concat(""))> (value |> string) </div>,
  };
};

let make = (_children) => {
  ...component,
  initialState: () => {
    buffer: [],
    mods: Eval.SModMap.empty,
    minibuffer: SExp.List([]),
  },
  reducer: (action, state) =>
    switch (action) {
    | ClearBuffer => Update({...state, buffer: []})
    | AppendBuffer(data, source) =>
      Update({
        ...state,
        buffer: [
          {data, source, time: Js.Date.make() |> Js.Date.toDateString},
          ...state.buffer,
        ],
      })
    | Update(minibuffer) => Update({...state, minibuffer})
    | Execute =>
      UpdateWithSideEffects({...state, minibuffer: SExp.List([])},
        (
          self =>
            switch (EvelInstance.eval(self, state.minibuffer)) {
            | Eval.Result(exp) => AppendBuffer(exp, "success") |> self.send
            | Eval.Error(exp) => AppendBuffer(exp, "error") |> self.send
            }
        ),
      )
    | _ => NoUpdate
    },
  render: self => {
    let length = self.state.buffer |> List.length;
    <div className="terminal">
      <div className="buffer">
        (
          self.state.buffer
          |> List.mapi((i, {data: datax, source, time}) =>
               <div className="log" key=(length - i |> string_of_int)>
                 <Label clazz=["source"] value=source />
                 <Label clazz=["time"] value=time />
                 <SExpViewer data=datax />
               </div>
             )
          |> Array.of_list
          |> array
        )
      </div>
      <div className="mini-buffer">
        <SExpEditor
          data=self.state.minibuffer
          onUpdate=(data => Update(data) |> self.send)
        />
        <button onClick=((_) => Execute |> self.send)>
          ("eval" |> string)
        </button>
      </div>
    </div>;
  },
};