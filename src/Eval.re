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
  let (?=): (t, DefineMap.key) => option(definition);
};

type result =
  | Result(SExp.t)
  | Error(SExp.t);

module Make = (Ctx: Context) : {let eval: (Ctx.t, SExp.t) => result;} => {
  module StringMap = Map.Make(String);
  let eval = ctx =>
    fun
    | SExp.Atom(_) as src => Result(src)
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
        Ctx.(ctx << SExp.List(next));
        Result(SExp.empty);
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
    | SExp.List([SExp.Atom("define"), SExp.Atom(name), ...body]) => {
        Ctx.(ctx <~ (name, [] ==> body));
        Result(
          SExp.List([
            SExp.Atom("defined"),
            SExp.List([SExp.Atom("quote"), SExp.Atom(name)]),
            SExp.List([SExp.Atom("quote"), SExp.List([])]),
            SExp.List([SExp.Atom("quote"), SExp.List(body)]),
          ]),
        );
      }
    | _ => Error(SExp.Atom("NotFound"));
};