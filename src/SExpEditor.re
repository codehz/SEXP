open ReasonReact;

type direction =
  | Current
  | Backward
  | Forward;

let directionToString =
  fun
  | Current => "Current"
  | Backward => "Backward"
  | Forward => "Forward";

type path =
  | RootPath
  | SimplePath(list(int))
  | RelativePath(list(int), direction);

let pathListToString = path =>
  path
  |> List.map(string_of_int)
  |> String.concat(":")
  |> (x => "[" ++ x ++ "]");

let pathToString =
  fun
  | RootPath => "root"
  | SimplePath(path) => "simple " ++ pathListToString(path)
  | RelativePath(path, dir) =>
    "relative " ++ pathListToString(path) ++ "@" ++ directionToString(dir);

let simplifyPath: (SExp.t, path) => list(int) =
  context =>
    fun
    | RootPath => []
    | SimplePath(result) => result
    | RelativePath(path, dir) => {
        let rec proc = prev => (
          fun
          | ([], _) => prev
          | ([pos], SExp.List(list))
              when dir == Current && List.length(list) > pos => [
              pos,
              ...prev,
            ]
          | ([pos], SExp.List(list))
              when dir == Forward && List.length(list) > pos + 1 => [
              pos + 1,
              ...prev,
            ]
          | ([pos], SExp.List(_)) when dir == Backward && pos == 0 => prev
          | ([pos], SExp.List(_)) when dir == Backward => [pos - 1, ...prev]
          | ([_], SExp.List(_)) when dir == Forward => prev
          | ([_], _) => prev
          | ([pos, ...rest], SExp.List(list)) when List.length(list) > pos =>
            proc([pos, ...prev], (rest, List.nth(list, pos)))
          | ([_, ..._], _) => prev
        );
        proc([], (path |> List.rev, context));
      };

type state = {
  select: path,
};

exception Overflow;

exception InvalidPath(path);

type action =
  | Select(path)
  | Append(path, direction)
  | AppendWithNil(path, direction)
  | Delete(path, direction)
  | Package(path)
  | Unpackage(path)
  | AsNil(path)
  | Modify(path, string);

type updateRequest =
  | CleanUpdate(path)
  | DirtyUpdate(SExp.t)
  | MixUpdate(path, SExp.t);

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

let doFocus: (bool, Js.nullable(Dom.element)) => unit = [%bs.raw
  {|
    function (choice, e) {
      if (e) {
        if (choice)
          e.focus();
        e.tabIndex=0;
      }
    }
  |}
];

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

let insertList: ('a, int, list('a)) => list('a) =
  (item, pos, list) => {
    let rec proc = prev =>
      fun
      | (0, next) => List.rev(prev) @ [item, ...next]
      | (_, []) => raise(Overflow)
      | (n, [hd, ...tl]) => proc([hd, ...prev], (n - 1, tl));
    proc([], (pos, list));
  };

let removeFromList: (int, list('a)) => list('a) =
  (pos, list) => {
    let rec proc = prev =>
      fun
      | (_, []) => raise(Overflow)
      | (0, [_, ...tl]) => List.rev(prev) @ tl
      | (n, [hd, ...tl]) => proc([hd, ...prev], (n - 1, tl));
    proc([], (pos, list));
  };

let pureDebug = (msgfn, thing) => {
  Js.log(msgfn(thing));
  thing;
};

let actionDump =
  fun
  | Select(path) => "Select " ++ pathToString(path)
  | Append(path, dir) =>
    "Append " ++ pathToString(path) ++ " " ++ directionToString(dir)
  | AppendWithNil(path, dir) =>
    "AppendWithNil " ++ pathToString(path) ++ " " ++ directionToString(dir)
  | Delete(path, dir) =>
    "Delete " ++ pathToString(path) ++ " " ++ directionToString(dir)
  | Package(path) => "Package " ++ pathToString(path)
  | Unpackage(path) => "Unpackage " ++ pathToString(path)
  | AsNil(path) => "AsNil " ++ pathToString(path)
  | Modify(path, text) => "Modify " ++ pathToString(path) ++ ": " ++ text;

let make = (~data: SExp.t, ~onUpdate, _children) => {
  ...component,
  initialState: () => {select: RootPath},
  reducer: (action: action, _state: state) => {
    action |> actionDump |> Js.log;
    let handleUpdate =
      fun
      | CleanUpdate(path) => Update({select: path})
      | DirtyUpdate(expr) =>
        SideEffects(((_) => onUpdate(expr)))
      | MixUpdate(path, expr) =>
        UpdateWithSideEffects(
          {select: path},
          ((_) => onUpdate(expr)),
        );
    switch (action) {
    | Select(path) => handleUpdate(CleanUpdate(path))
    | Append(path, direction) =>
      let spath = path |> simplifyPath(data) |> List.rev;
      let rec access = fn => (
        fun
        /* root */
        | ([], SExp.List([]), []) => (
            SExp.List([SExp.Atom("")]) |> fn,
            SimplePath([0]),
          )
        | ([], SExp.Atom(name), []) => (
            SExp.List([SExp.Atom(name)]) |> fn,
            SimplePath([0]),
          )
        | ([], SExp.List(list), []) => (
            SExp.List([SExp.Atom(""), ...list]) |> fn,
            SimplePath([0]),
          )
        | ([pos], SExp.List(list), prev) =>
          switch (direction) {
          | Forward => (
              SExp.List(insertList(SExp.Atom(""), pos + 1, list)) |> fn,
              SimplePath([pos + 1, ...prev]),
            )
          | _ => (
              SExp.List(insertList(SExp.Atom(""), pos, list)) |> fn,
              SimplePath([pos, ...prev]),
            )
          }
        | ([pos, ...next], SExp.List(list), prev) =>
          access(
            newitem =>
              SExp.List(
                list
                |> List.mapi((i, item) =>
                     if (i == pos) {
                       newitem;
                     } else {
                       item;
                     }
                   ),
              )
              |> fn,
            (next, pos |> List.nth(list), [pos, ...prev]),
          )
        | _ => raise(InvalidPath(path))
      );
      let (expr, path) = access(x => x, (spath, data, []));
      handleUpdate(MixUpdate(path, expr));
    | AppendWithNil(path, direction) =>
      let spath = path |> simplifyPath(data) |> List.rev;
      let rec access = fn => (
        fun
        /* root */
        | ([], SExp.List([]), []) => (
            SExp.List([SExp.Atom("")]) |> fn,
            SimplePath([0]),
          )
        | ([], SExp.Atom(name), []) => (
            SExp.List([SExp.Atom(name)]) |> fn,
            SimplePath([0]),
          )
        | ([], SExp.List(list), []) => (
            SExp.List([SExp.Atom(""), ...list]) |> fn,
            SimplePath([0]),
          )
        | ([pos], SExp.List(list), prev) => {
            let xpos = direction == Forward ? pos + 1 : pos;
            (
              SExp.List(
                insertList(
                  SExp.Atom(""),
                  xpos,
                  list
                  |> List.mapi((i, item) =>
                       if (i == pos) {
                         SExp.List([]);
                       } else {
                         item;
                       }
                     ),
                ),
              )
              |> fn,
              SimplePath([xpos, ...prev]),
            );
          }
        | ([pos, ...next], SExp.List(list), prev) =>
          access(
            newitem =>
              SExp.List(
                list
                |> List.mapi((i, item) =>
                     if (i == pos) {
                       newitem;
                     } else {
                       item;
                     }
                   ),
              )
              |> fn,
            (next, pos |> List.nth(list), [pos, ...prev]),
          )
        | _ => raise(InvalidPath(path))
      );
      let (expr, path) = access(x => x, (spath, data, []));
      handleUpdate(MixUpdate(path, expr));
    | Delete(path, dir) =>
      let spath = path |> simplifyPath(data) |> List.rev;
      let rec access = fn => (
        fun
        | ([], SExp.List(_) as src) => (src |> fn, RootPath)
        | ([], SExp.Atom(_)) => (SExp.List([]) |> fn, RootPath)
        | ([pos], SExp.List(list)) when List.length(list) > pos => (
            SExp.List(removeFromList(pos, list)) |> fn,
            dir == Backward ?
              RelativePath(spath |> List.rev, Backward) : RelativePath(spath |> List.rev, Current),
          )
        | ([_], SExp.List(_) as list) => (list |> fn, SimplePath(spath |> List.rev))
        | ([pos, ...next], SExp.List(list)) =>
          access(
            newitem =>
              SExp.List(
                list
                |> List.mapi((i, item) =>
                     if (i == pos) {
                       newitem;
                     } else {
                       item;
                     }
                   ),
              )
              |> fn,
            (next, pos |> List.nth(list)),
          )
        | _ => raise(InvalidPath(path))
      );
      let (expr, path) = access(x => x, (spath, data));
      handleUpdate(MixUpdate(path, expr));
    | Package(path) =>
      let spath = path |> simplifyPath(data) |> List.rev;
      let rec access = fn => (
        fun
        /* root */
        | ([], SExp.List([]), prev) => (
            SExp.List([SExp.Atom("")]) |> fn,
            SimplePath([0, ...prev]),
          )
        | ([], SExp.Atom(""), prev) => (
            SExp.List([SExp.Atom("")]) |> fn,
            SimplePath([0, ...prev]),
          )
        | ([], src, prev) => (
            SExp.List([src, SExp.Atom("")]) |> fn,
            SimplePath([1, ...prev]),
          )
        | ([pos, ...next], SExp.List(list), prev) =>
          access(
            newitem =>
              SExp.List(
                list
                |> List.mapi((i, item) =>
                     if (i == pos) {
                       newitem;
                     } else {
                       item;
                     }
                   ),
              )
              |> fn,
            (next, pos |> List.nth(list), [pos, ...prev]),
          )
        | _ => raise(InvalidPath(path))
      );
      let (expr, path) = access(x => x, (spath, data, []));
      handleUpdate(MixUpdate(path, expr));
    | Unpackage(path) =>
      let spath = path |> simplifyPath(data) |> List.rev;
      let rec access = fn => (
        fun
        | ([], src) => src |> fn
        | ([pos], SExp.List(list)) => {
            let rec proc = prev => (
              fun
              | (0, [SExp.List(list), ...next]) =>
                List.rev(prev) @ list @ next
              | (0, next) => List.rev(prev) @ next
              | (n, [hd, ...tl]) => proc([hd, ...prev], (n - 1, tl))
              | (_, []) => prev
            );
            SExp.List(proc([], (pos, list))) |> fn;
          }
        | ([pos, ...next], SExp.List(list)) =>
          access(
            newitem =>
              SExp.List(
                list
                |> List.mapi((i, item) =>
                     if (i == pos) {
                       newitem;
                     } else {
                       item;
                     }
                   ),
              )
              |> fn,
            (next, pos |> List.nth(list)),
          )
        | _ => raise(InvalidPath(path))
      );
      let expr = access(x => x, (spath, data));
      handleUpdate(DirtyUpdate(expr));
    | AsNil(path) =>
      let spath = path |> simplifyPath(data) |> List.rev;
      let rec access = fn => (
        fun
        | ([], SExp.Atom("")) => SExp.List([]) |> fn
        | ([], src) => src |> fn
        | ([pos, ...next], SExp.List(list)) =>
          access(
            newitem =>
              SExp.List(
                list
                |> List.mapi((i, item) =>
                     if (i == pos) {
                       newitem;
                     } else {
                       item;
                     }
                   ),
              )
              |> fn,
            (next, pos |> List.nth(list)),
          )
        | _ => raise(InvalidPath(path))
      );
      let expr = access(x => x, (spath, data));
      handleUpdate(DirtyUpdate(expr));
    | Modify(path, text) =>
      let spath = path |> simplifyPath(data) |> List.rev;
      let rec access = fn => (
        fun
        | ([], SExp.Atom(_)) => SExp.Atom(text) |> fn
        | ([pos, ...next], SExp.List(list)) =>
          access(
            newitem =>
              SExp.List(
                list
                |> List.mapi((i, item) =>
                     if (i == pos) {
                       newitem;
                     } else {
                       item;
                     }
                   ),
              )
              |> fn,
            (next, pos |> List.nth(list)),
          )
        | _ => raise(InvalidPath(path))
      );
      let expr = access(x => x, (spath, data));
      handleUpdate(DirtyUpdate(expr));
    };
  },
  render: self => {
    let path = self.state.select |> simplifyPath(data);
    let handleFocus = (path, event) => {
      ReactEventRe.Focus.stopPropagation(event);
      Select(SimplePath(path)) |> self.send;
    };
    let handleKeydown = (path, event) => {
      ReactEventRe.Keyboard.stopPropagation(event);
      let proc = (_) => ReactEventRe.Keyboard.preventDefault(event);
      switch (
        ReactEventRe.Keyboard.keyCode(event),
        ReactEventRe.Keyboard.ctrlKey(event),
      ) {
      | (32, true) =>
        Append(SimplePath(path), Backward) |> self.send |> proc
      | (32, false) =>
        Append(SimplePath(path), Forward) |> self.send |> proc
      | (13, _) => Package(SimplePath(path)) |> self.send |> proc
      | _ => ()
      };
    };
    let handleKeyup = (path, event) => {
      ReactEventRe.Keyboard.stopPropagation(event);
      let proc = (_) => ReactEventRe.Keyboard.preventDefault(event);
      switch (ReactEventRe.Keyboard.keyCode(event)) {
      | 8 => Delete(SimplePath(path), Backward) |> self.send |> proc
      | 46 => Delete(SimplePath(path), Forward) |> self.send |> proc
      | 38 => Select(RelativePath(path, Backward)) |> self.send |> proc
      | 40 => Select(RelativePath(path, Forward)) |> self.send |> proc
      | _ => ()
      };
    };
    let renderAtom = (xpath, value) => {
      let handleInputKeyup = event => {
        ReactEventRe.Keyboard.stopPropagation(event);
        let proc = (_) => ReactEventRe.Keyboard.preventDefault(event);
        switch (
          event |> getValueByKeyboard,
          ReactEventRe.Keyboard.keyCode(event),
        ) {
        | (_, 38) =>
          Select(RelativePath(path, Backward)) |> self.send |> proc
        | (_, 40) =>
          Select(RelativePath(path, Forward)) |> self.send |> proc
        | ("", _) => handleKeyup(xpath, event)
        | _ => ()
        };
      };
      let handleInputKeydown = event => {
        ReactEventRe.Keyboard.stopPropagation(event);
        let proc = (_) => ReactEventRe.Keyboard.preventDefault(event);
        switch (
          event |> getValueByKeyboard,
          ReactEventRe.Keyboard.keyCode(event),
          ReactEventRe.Keyboard.key(event),
        ) {
        | (_, _, "(")
        | (_, _, ")")
        | (_, 38, _)
        | (_, 40, _) => ReactEventRe.Keyboard.preventDefault(event)
        | ("", 32, _) =>
          AppendWithNil(
            SimplePath(path),
            ReactEventRe.Keyboard.ctrlKey(event) ? Backward : Forward,
          )
          |> self.send
          |> proc
        | (_, 32, _) =>
          Append(
            SimplePath(path),
            ReactEventRe.Keyboard.ctrlKey(event) ? Backward : Forward,
          )
          |> self.send
          |> proc
        | (_, 13, _) => Package(SimplePath(path)) |> self.send |> proc
        | _ => ()
        };
      };
      let handleBlur = event =>
        switch (event |> getValueByFocus) {
        | "" when path == xpath =>
          Delete(SimplePath(path), Backward) |> self.send
        | _ => ()
        };
      let handleChange = event =>
        Modify(SimplePath(xpath), event |> getValue) |> self.send;
      let handleInputFocus = event => {
        selectAll(event);
        handleFocus(xpath, event);
      };
      <ContentEditable
        className="atom editor"
        html=value
        autofocus=(path == xpath)
        onKeyUp=handleInputKeyup
        onBlur=handleBlur
        onChange=handleChange
        onKeyDown=handleInputKeydown
        onFocus=handleInputFocus
      />;
    };
    let rec renderList = (xpath, list) => {
      let handleListKeyup = event => {
        ReactEventRe.Keyboard.stopPropagation(event);
        let proc = (_) => ReactEventRe.Keyboard.preventDefault(event);
        switch (
          ReactEventRe.Keyboard.shiftKey(event),
          ReactEventRe.Keyboard.keyCode(event),
        ) {
        | (true, 8) => Unpackage(SimplePath(xpath)) |> self.send |> proc
        | _ => handleKeyup(xpath, event)
        };
      };
      <span
        className="list"
        ref=(doFocus(path == xpath))
        onKeyDown=(handleKeydown(xpath))
        onKeyUp=handleListKeyup
        onFocus=(handleFocus(xpath))>
        (
          list
          |> List.mapi((i, item) => {
               let clazz =
                 switch (item) {
                 | SExp.List([]) => "item item-nil"
                 | _ => "item"
                 };
               <div key=(i |> string_of_int) className=clazz>
                 (item |> renderSExp([i, ...xpath]))
               </div>;
             })
          |> Array.of_list
          |> array
        )
      </span>;
    }
    and renderSExp = xpath =>
      fun
      | SExp.Atom(value) => value |> renderAtom(xpath)
      | SExp.List([SExp.Atom("quote" as special), content])
      | SExp.List([SExp.Atom("string" as special), SExp.List(_) as content]) =>
        <span
          className=special
          ref=(doFocus(path == xpath))
          onKeyDown=(handleKeydown(xpath))
          onKeyUp=(handleKeyup(xpath))
          onFocus=(handleFocus(xpath))>
          (content |> renderSExp([1, ...xpath]))
        </span>
      | SExp.List([]) =>
        <span
          className="nil"
          ref=(doFocus(path == xpath))
          onKeyDown=(handleKeydown(xpath))
          onKeyUp=(handleKeyup(xpath))
          onFocus=(handleFocus(xpath))
        />
      | SExp.List(list) => list |> renderList(xpath);
    renderSExp([], data);
  },
};