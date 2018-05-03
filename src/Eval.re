exception Imposible;

exception Invalid;

module DefineMap = Map.Make(String);

type definition = {
  param: list(string),
  body: SExp.t,
};

type define = DefineMap.t(definition);

type promptPack('t) = Prompt('t, string);

module type Context = {
  type t;
  let clear: t => unit;
  let (<<): (t, SExp.t) => unit;
  let (>>): (promptPack(t), SExp.t => unit) => unit;
  let (<==): (t, (DefineMap.key, definition)) => unit;
  let (==>): (t, DefineMap.key) => option(definition);
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
        Result(SExp.List([]));
      }
    | SExp.List([SExp.Atom("clear")]) => {
      ctx |> Ctx.clear;
        Result(SExp.List([]));
      }
    | _ => Error(SExp.Atom("NotFound"));
};