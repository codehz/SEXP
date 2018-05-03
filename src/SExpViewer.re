open ReasonReact;

let component = statelessComponent("SExpViewer");

let make = (~data: SExp.t, _children) => {
  ...component,
  render: _self => {
    let renderAtom = value =>
      <span className="atom"> (value |> string) </span>;
    let rec renderList = list =>
      <span className="list">
        (
          list
          |> List.mapi((i, item) => {
               let clazz =
                 switch (item) {
                 | SExp.List([]) => "item item-nil"
                 | _ => "item"
                 };
               <div key=(i |> string_of_int) className=clazz>
                 (item |> renderSExp)
               </div>;
             })
          |> Array.of_list
          |> array
        )
      </span>
    and renderSExp =
      fun
      | SExp.Atom(value) => value |> renderAtom
      | SExp.List([SExp.Atom("quote" as special), content])
      | SExp.List([SExp.Atom("string" as special), SExp.List(_) as content]) =>
        <span className=special> (content |> renderSExp) </span>
      | SExp.List([]) => <span className="nil" />
      | SExp.List(list) => list |> renderList;
    renderSExp(data);
  },
};