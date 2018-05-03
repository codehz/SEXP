exception Imposible;

exception Invalid;

module SModMap = Map.Make(String);

type definition = {
  param: list(string),
  body: SExp.t,
};

type smod = SModMap.t(definition);

module type Context = {
  type t;
  let write: (t, SExp.t) => unit;
  let prompt: (t, string, SExp.t => unit) => unit;
  let defineModule: (t, SModMap.key, definition) => unit;
  let loadModule: (t, SModMap.key) => option(definition);
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
    | _ => Error(SExp.Atom("NotFound"));
};