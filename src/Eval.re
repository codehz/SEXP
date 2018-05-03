exception Imposible;

exception Invalid;

module DefineMap = Map.Make(String);

type definition = {
  param: list(string),
  body: SExp.t,
};

type define = DefineMap.t(definition);

module type Context = {
  type t;
  let clear: t => unit;
  let write: (t, SExp.t) => unit;
  let prompt: (t, string, SExp.t => unit) => unit;
  let define: (t, DefineMap.key, definition) => unit;
  let acquire: (t, DefineMap.key) => option(definition);
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
        Ctx.write(ctx, SExp.List(next));
        Result(SExp.List([]));
      }
    | SExp.List([SExp.Atom("clear")]) => {
        Ctx.clear(ctx);
        Result(SExp.List([]));
      }
    | _ => Error(SExp.Atom("NotFound"));
};