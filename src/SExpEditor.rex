open ReasonReact;

type expPath =
  | Here
  | Index(int, expPath);

let rec debugPath = prev =>
  fun
  | Here => prev ++ "!"
  | Index(index, path) =>
    debugPath(prev ++ ":" ++ string_of_int(index), path);

type selection =
  | Selection(expPath, int);

let rec (>=>) = next =>
  fun
  | Here => Index(next, Here)
  | Index(index, src) => Index(index, next >=> src);

let nothing = _n => ();

let rec nth = i =>
  fun
  | [head, ..._] when i == 0 => Some(head)
  | [_, ...tail] => tail |> nth(i - 1)
  | _ => None;

type state = {
  exp: SExp.t,
  focus: option(expPath),
};

type actionDirection =
  | Forward
  | Backward;

type action =
  | Focus(expPath)
  | Cancel
  | Modify(expPath, string)
  | Append(expPath, actionDirection)
  | Insert(expPath)
  | WrapIt(expPath)
  | Replace(expPath, SExp.t, bool)
  | UnBracket(expPath)
  | Remove(expPath, actionDirection);

let component = reducerComponent("SExpEditor");

let getValue = event => ReactDOMRe.domElementToObj(
                          ReactEventRe.Form.target(event),
                        )##value;

let getValueByFocus = event => ReactDOMRe.domElementToObj(
                                 ReactEventRe.Focus.target(event),
                               )##innerText;

let getValueByKeyboard = event => ReactDOMRe.domElementToObj(
                                    ReactEventRe.Keyboard.target(event),
                                  )##innerText;

let selectAll: ReactEventRe.Focus.t => unit = [%bs.raw
  {|
    function (event) {
      const self = event.target;
      const selection = window.getSelection();
      const range = document.createRange();
      range.selectNodeContents(self);
      selection.removeAllRanges();
      selection.addRange(range);
    }
  |}
];

let doFocus: Js.nullable(Dom.element) => unit = [%bs.raw
  {|
    function (e) {
      if (e) e.focus();
    }
  |}
];

let passFocus: ReactEventRe.Focus.t => unit = [%bs.raw
  {|
    function ({target: e}) {
      if (e && e.firstChild) e.firstChild.focus();
    }
  |}
];

exception InvalidPath(int, string);

exception Impossible;

let make = (~data: SExp.t, ~onUpdate, _children) => {
  ...component,
  initialState: () => {exp: data, focus: None},
  reducer: (action, state) => {
    let doUpdate = exp =>
      UpdateWithSideEffects(
        {...state, exp},
        self => onUpdate(self.state.exp),
      );
    let doUpdateWithFocus = (exp, focus) =>
      UpdateWithSideEffects(
        {exp, focus},
        self => onUpdate(self.state.exp),
      );
    switch (action) {
    | Focus(path) => Update({...state, focus: Some(path)})
    | Cancel => Update({...state, focus: None})
    | Modify(path, exp) =>
      let rec access = item => (
        fun
        | Here => SExp.Atom(exp)
        | Index(index, path) =>
          switch (item) {
          | SExp.List(list) when List.length(list) > index =>
            SExp.List(
              list
              |> List.mapi((n, v) =>
                   if (n == index) {
                     access(v, path);
                   } else {
                     v;
                   }
                 ),
            )
          | x => raise(InvalidPath(__LINE__, SExp.toString(x)))
          }
      );
      doUpdate(access(state.exp, path));
    | Append(path, dir) =>
      let rec access = item => (
        fun
        | Here => SExp.List([SExp.Atom("")])
        | Index(index, path) =>
          switch (item) {
          | SExp.List(list) when List.length(list) > index =>
            switch (path) {
            | Here =>
              let rec sp = (
                fun
                | (_n, [], _) => raise(Impossible)
                | (0, [head, ...tail], prev) =>
                  List.rev(prev)
                  @ (
                    if (dir == Forward) {
                      [head, SExp.Atom(""), ...tail];
                    } else {
                      [SExp.Atom(""), head, ...tail];
                    }
                  )
                | (n, [head, ...tail], prev) =>
                  (n - 1, tail, [head, ...prev]) |> sp
              );
              SExp.List(sp((index, list, [])));
            | _ =>
              SExp.List(
                list
                |> List.mapi((n, v) =>
                     if (n == index) {
                       access(v, path);
                     } else {
                       v;
                     }
                   ),
              )
            }
          | x => raise(InvalidPath(__LINE__, SExp.toString(x)))
          }
      );
      let rec getFocus = (
        fun
        | Here => Index(0, Here)
        | Index(index, Here) => Index(index + 1, Here)
        | Index(index, path) => Index(index, path |> getFocus)
      );
      doUpdateWithFocus(
        access(state.exp, path),
        Some(
          if (dir == Forward) {
            path |> getFocus;
          } else {
            path;
          },
        ),
      );
    | Insert(path) =>
      let rec access = item => (
        fun
        | Here =>
          switch (item) {
          | SExp.Atom(_) as src => SExp.List([src])
          | SExp.List(list) => SExp.List([SExp.Atom(""), ...list])
          }
        | Index(index, path) =>
          switch (item) {
          | SExp.List(list) when List.length(list) > index =>
            SExp.List(
              list
              |> List.mapi((n, v) =>
                   if (n == index) {
                     access(v, path);
                   } else {
                     v;
                   }
                 ),
            )
          | x => raise(InvalidPath(__LINE__, SExp.toString(x)))
          }
      );
      let rec getFocus = (
        fun
        | Here => Index(0, Here)
        | Index(index, path) => Index(index, path |> getFocus)
      );
      doUpdateWithFocus(access(state.exp, path), Some(getFocus(path)));
    | WrapIt(path) =>
      let rec access = item => (
        fun
        | Here => SExp.List([item, SExp.Atom("")])
        | Index(index, path) =>
          switch (item) {
          | SExp.List(list) when List.length(list) > index =>
            SExp.List(
              list
              |> List.mapi((n, v) =>
                   if (n == index) {
                     access(v, path);
                   } else {
                     v;
                   }
                 ),
            )
          | x => raise(InvalidPath(__LINE__, SExp.toString(x)))
          }
      );
      let rec getFocus = (
        fun
        | Here => Index(1, Here)
        | Index(index, path) => Index(index, path |> getFocus)
      );
      doUpdateWithFocus(access(state.exp, path), Some(getFocus(path)));
    | Replace(path, target, false) =>
      let rec access = item => (
        fun
        | Here => target
        | Index(index, path) =>
          switch (item) {
          | SExp.List(list) when List.length(list) > index =>
            SExp.List(
              list
              |> List.mapi((n, v) =>
                   if (n == index) {
                     access(v, path);
                   } else {
                     v;
                   }
                 ),
            )
          | x => raise(InvalidPath(__LINE__, SExp.toString(x)))
          }
      );
      doUpdate(access(state.exp, path));
    | Replace(path, target, true) =>
      let rec access = item => (
        fun
        | Here => SExp.List([target, SExp.Atom("")])
        | Index(index, path) =>
          switch (item) {
          | SExp.List(list) when List.length(list) > index =>
            switch (path) {
            | Here =>
              let rec sp = (
                fun
                | (_n, [], _) => raise(Impossible)
                | (0, [_head, ...tail], prev) =>
                  List.rev(prev) @ [target, SExp.Atom(""), ...tail]
                | (n, [head, ...tail], prev) =>
                  (n - 1, tail, [head, ...prev]) |> sp
              );
              SExp.List(sp((index, list, [])));
            | _ =>
              SExp.List(
                list
                |> List.mapi((n, v) =>
                     if (n == index) {
                       access(v, path);
                     } else {
                       v;
                     }
                   ),
              )
            }
          | x => raise(InvalidPath(__LINE__, SExp.toString(x)))
          }
      );
      let rec getFocus = (
        fun
        | Here => Index(0, Here)
        | Index(index, Here) => Index(index + 1, Here)
        | Index(index, path) => Index(index, path |> getFocus)
      );
      doUpdateWithFocus(access(state.exp, path), Some(getFocus(path)));
    | UnBracket(path) =>
      let rec access = item => (
        fun
        | Here => item
        | Index(index, path) =>
          switch (item) {
          | SExp.List(list) when List.length(list) > index =>
            switch (path) {
            | Here =>
              let rec sp = (
                fun
                | (_n, [], _) => raise(Impossible)
                | (0, [SExp.List(content), ...tail], prev) =>
                  List.rev(prev) @ content @ tail
                | (0, [_, ..._], _) => list
                | (n, [head, ...tail], prev) =>
                  (n - 1, tail, [head, ...prev]) |> sp
              );
              SExp.List(sp((index, list, [])));
            | _ =>
              SExp.List(
                list
                |> List.mapi((n, v) =>
                     if (n == index) {
                       access(v, path);
                     } else {
                       v;
                     }
                   ),
              )
            }
          | x => raise(InvalidPath(__LINE__, SExp.toString(x)))
          }
      );
      doUpdateWithFocus(access(state.exp, path), Some(path));
    | Remove(path, direction) =>
      let rec access = item => (
        fun
        | Here => item
        | Index(index, path) =>
          switch (item) {
          | SExp.List(list) when List.length(list) > index =>
            switch (path) {
            | Here =>
              let rec sp = (
                fun
                | (_n, [], _) => raise(Impossible)
                | (0, [_head, ...tail], prev) => List.rev(prev) @ tail
                | (n, [head, ...tail], prev) =>
                  (n - 1, tail, [head, ...prev]) |> sp
              );
              SExp.List(sp((index, list, [])));
            | _ =>
              SExp.List(
                list
                |> List.mapi((n, v) =>
                     if (n == index) {
                       access(v, path);
                     } else {
                       v;
                     }
                   ),
              )
            }
          | _ => item
          }
      );
      let rec getFocus = (
        fun
        | Here => Here
        | Index(index, Here) => Index(index - 1, Here)
        | Index(index, path) => Index(index, path |> getFocus)
      );
      doUpdateWithFocus(
        access(state.exp, path),
        Some(direction == Forward ? path : getFocus(path)),
      );
    };
  },
  render: self => {
    let backspaceToDelete = (path, fn, event) => {
      ReactEventRe.Keyboard.stopPropagation(event);
      switch (ReactEventRe.Keyboard.keyCode(event)) {
      | 8 when ReactEventRe.Keyboard.ctrlKey(event) =>
        UnBracket(path) |> self.send
      | 8 => Remove(path, Backward) |> self.send
      | 46 => Remove(path, Forward) |> self.send
      | code => fn(code)
      };
    };
    let spaceAndEnterAction = (path, event) => {
      let clearEvent = (_) => {
        ReactEventRe.Keyboard.preventDefault(event);
        ReactEventRe.Keyboard.stopPropagation(event);
      };
      let rec getParent =
        fun
        | Here => Here
        | Index(_, Here) => Here
        | Index(n, rest) => Index(n, rest |> getParent);
      let rec getFocus = dir =>
        fun
        | Here => Here
        | Index(n, Here) => Index(n + (dir == Forward ? 1 : (-1)), Here)
        | Index(n, rest) => Index(n, rest |> getFocus(dir));
      switch (
        ReactEventRe.Keyboard.keyCode(event),
        ReactEventRe.Keyboard.shiftKey(event),
        ReactEventRe.Keyboard.ctrlKey(event),
      ) {
      /* space */
      | (32, shift, false) =>
        Append(path, shift ? Backward : Forward) |> self.send |> clearEvent
      | (32, false, true) =>
        Focus(path |> getParent) |> self.send |> clearEvent
      /* Enter */
      | (13, false, false) => Insert(path) |> self.send |> clearEvent
      | (13, false, true) => WrapIt(path) |> self.send |> clearEvent
      /* Up */
      | (38, false, false)
      /* Left */
      | (37, false, true) =>
        Focus(path |> getFocus(Backward)) |> self.send |> clearEvent
      /* Down */
      | (40, false, false)
      /* Right */
      | (39, false, true) =>
        Focus(path |> getFocus(Forward)) |> self.send |> clearEvent
      | _ => ()
      };
    };
    let renderAtom = (path, rvalue) =>
      <ContentEditable
        className="atom editor"
        html=rvalue
        autofocus=(self.state.focus == Some(path))
        onFocus=selectAll
        onKeyUp=(
          event => {
            ReactEventRe.Keyboard.stopPropagation(event);
            if (event |> getValueByKeyboard == "") {
              backspaceToDelete(
                path,
                fun
                | 27 => Remove(path, Forward) |> self.send
                | _ => (),
                event,
              );
            };
          }
        )
        onKeyDown=(
          event => {
            ReactEventRe.Keyboard.stopPropagation(event);
            if (event |> getValueByKeyboard != "") {
              spaceAndEnterAction(path, event);
            } else {
              switch (ReactEventRe.Keyboard.keyCode(event)) {
              | 32 =>
                ReactEventRe.Keyboard.preventDefault(event);
                Replace(path, SExp.List([]), true) |> self.send;
              | 13 =>
                ReactEventRe.Keyboard.preventDefault(event);
                Replace(path, SExp.List([]), false) |> self.send;
              | _ => ()
              };
            };
          }
        )
        onBlur=(
          event =>
            if (event |> getValueByFocus == "") {
              Remove(path, Forward) |> self.send;
            } else {
              Cancel |> self.send;
            }
        )
        onChange=(event => Modify(path, event |> getValue) |> self.send)
      />;
    let rec renderList = (path, list) => {
      list
      |> List.mapi((i, item) => {
           let clazz =
             switch (item) {
             | SExp.List([]) => "item item-nil"
             | _ => "item"
             };
           <div
             key=(i |> string_of_int) className=clazz>
             (i >=> path <~ item)
           </div>;
         })
      |> Array.of_list
      |> array
      |> (
        child =>
          switch (self.state.focus) {
          | Some(xpath)
              when
                xpath == path
                || xpath == (list |> List.length >=> path)
                || xpath == ((-1) >=> path) =>
            <span
              className="list"
              tabIndex=0
              onKeyDown=(spaceAndEnterAction(path))
              onKeyUp=(backspaceToDelete(path, nothing))
              ref=doFocus>
              child
            </span>
          | _ =>
            <span
              className="list"
              tabIndex=0
              onKeyDown=(spaceAndEnterAction(path))
              onKeyUp=(backspaceToDelete(path, nothing))>
              child
            </span>
          }
      );
    }
    and (<~) = path =>
      fun
      | SExp.Atom(rvalue) => renderAtom(path, rvalue)
      | SExp.List([SExp.Atom("quote" as special), content])
      | SExp.List([SExp.Atom("string" as special), SExp.List(_) as content]) =>
        switch (self.state.focus) {
        | Some(xpath)
            when
              xpath == path
              || xpath == (0 >=> path)
              || xpath == ((-1) >=> path) =>
          <span
            className=special
            tabIndex=0
            ref=doFocus
            onKeyUp=(backspaceToDelete(path, nothing))
            onKeyDown=(spaceAndEnterAction(path))>
            (1 >=> path <~ content)
          </span>
        | _ =>
          <span
            className=special
            tabIndex=0
            onKeyUp=(backspaceToDelete(path, nothing))
            onKeyDown=(spaceAndEnterAction(path))>
            (1 >=> path <~ content)
          </span>
        }
      | SExp.List([]) =>
        switch (self.state.focus) {
        | Some(xpath)
            when
              xpath == path
              || xpath == (0 >=> path)
              || xpath == ((-1) >=> path) =>
          <span
            className="nil"
            onKeyUp=(backspaceToDelete(path, nothing))
            onKeyDown=(spaceAndEnterAction(path))
            tabIndex=0
            ref=doFocus
          />
        | _ =>
          <span
            className="nil"
            onKeyUp=(backspaceToDelete(path, nothing))
            onKeyDown=(spaceAndEnterAction(path))
            tabIndex=0
          />
        }
      | SExp.List(list) => renderList(path, list);
    Here <~ data;
  },
};