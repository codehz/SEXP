exception Imposible;

exception Invalid;

module DefineMap = Map.Make(String);

type definition = {
  params: list(DefineMap.key),
  body: list(SExp.t),
};

let (==>) = (params, body) => {params, body};

type define = DefineMap.t(definition);

type promptPack('t) =
  | Prompt('t, string);

module type Context = {
  type t;
  let clear: t => unit;
  let (<<): (t, SExp.t) => unit;
  let (>>): (promptPack(t), SExp.t => unit) => unit;
  let (<~): (t, (DefineMap.key, definition)) => unit;
  let (%): (t, DefineMap.key) => option(definition);
};

type result =
  | Result(SExp.t)
  | Error(SExp.t);

let isValid = text =>
  Js.Re.test(
    text,
    [%re
      "/(?:-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?)|(?:true|false)|null/g"
    ],
  );

module Make = (Ctx: Context) : {let eval: (Ctx.t, SExp.t) => result;} => {
  module StringMap = Map.Make(String);
  let rec eval = ctx =>
    fun
    | SExp.List([]) as src => Result(src)
    | SExp.Atom(name) as src when isValid(name) => Result(src)
    | SExp.Atom(name) =>
      switch (Ctx.(ctx % name)) {
      | Some({params: [], body: [hd]}) => Result(hd)
      | Some({params, body}) =>
        Result(
          SExp.List([
            SExp.Atom("lambda"),
            SExp.List(params |> List.map(x => SExp.Atom(x))),
            SExp.List(body),
          ]),
        )
      | _ => Error(SExp.List([SExp.Atom("SymbolNotFound"), SExp.Atom(name)]))
      }
    | SExp.List([SExp.Atom("quote"), next]) => Result(next)
    | SExp.List([SExp.Atom("string"), SExp.List(list)]) =>
      Result(
        list
        |> List.map(
             fun
             | SExp.Atom(value) => value
             | SExp.List([]) => " "
             | _ => "",
           )
        |> String.concat("")
        |> (x => SExp.Atom(x)),
      )
    | SExp.List([SExp.Atom("debug"), ...next]) => {
        let vals =
          next
          |> List.map(eval(ctx))
          |> List.fold_left(
               (p, a) =>
                 switch (p, a) {
                 | (Result(SExp.List(list)), Result(src)) =>
                   Result(SExp.List([src, ...list]))
                 | (Error(_) as err, _)
                 | (_, Error(_) as err) => err
                 | (Result(_), _) => Error(SExp.Atom("InvalidEval"))
                 },
               Result(SExp.List([])),
             );
        switch (vals) {
        | Result(SExp.List(list)) =>
          Ctx.(ctx << SExp.List(list |> List.rev));
          Result(SExp.empty);
        | Result(_) => raise(Invalid)
        | Error(_) as err => err
        };
      }
    | SExp.List([SExp.Atom("clear")]) => {
        ctx |> Ctx.clear;
        Result(SExp.empty);
      }
    | SExp.List([
        SExp.Atom("define"),
        SExp.List([SExp.Atom(name), ...params]),
        ...body,
      ]) =>
      if (params
          |> List.for_all(
               fun
               | SExp.Atom(_) => true
               | _ => false,
             )) {
        Ctx.(
          ctx
          <~ (
            name,
            params
            |> List.map(
                 fun
                 | SExp.Atom(name) => name
                 | _ => raise(Invalid),
               )
            ==> body,
          )
        );
        Result(
          SExp.List([
            SExp.Atom("defined"),
            SExp.List([SExp.Atom("quote"), SExp.Atom(name)]),
            SExp.List([SExp.Atom("quote"), SExp.List(params)]),
            SExp.List([SExp.Atom("quote"), SExp.List(body)]),
          ]),
        );
      } else {
        Error(SExp.Atom("InvalidDefine"));
      }
    | SExp.List([SExp.Atom("define"), SExp.Atom(name), body]) =>
      switch (eval(ctx, body)) {
      | Result(rst) =>
        Ctx.(ctx <~ (name, [] ==> [rst]));
        Result(
          SExp.List([
            SExp.Atom("defined"),
            SExp.List([SExp.Atom("quote"), SExp.Atom(name)]),
            SExp.List([SExp.Atom("quote"), SExp.List([])]),
            SExp.List([SExp.Atom("quote"), rst]),
          ]),
        );
      | err => err
      }
    | _ => Error(SExp.Atom("NotFound"));
};