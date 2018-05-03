let expr =
  SExp.parse("(module test (define (quote main) (println (string (Hello, () world)))))");

module SyncView = {
  open ReasonReact;
  type state = {exp: SExp.t};
  type action =
    | Update(SExp.t);
  let component = reducerComponent("SyncView");
  let make = (~data: SExp.t, _children) => {
    ...component,
    initialState: () => {exp: data},
    reducer: (Update(exp), _state) => Update({exp: exp}),
    render: self =>
      <div className="SyncView">
        <SExpEditor
          data=self.state.exp
          onUpdate=(x => Update(x) |> self.send)
        />
        <pre> (self.state.exp |> SExp.toString |> string) </pre>
      </div>,
  };
};

ReactDOMRe.renderToElementWithId(<SyncView data=expr />, "app");
/* ReactDOMRe.renderToElementWithId(<Terminal />, "app"); */