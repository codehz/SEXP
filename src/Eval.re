exception Imposible;

exception Invalid;

type env = list((string, SExp.t));

let (>=>) = (a, b) => List.rev_append(a, b);

let purgeEnv = (num, env) => {
  let rec loop = (
    fun
    | (0, list) => list
    | (n, [_, ...tl]) => (n - 1, tl) |> loop
    | _ => raise(Invalid)
  );
  (num, env |> List.rev) |> loop |> List.rev;
};

type promptPack('t) =
  | Prompt('t, string);

module type Context = {
  type t;
  let clear: t => unit;
  let (<<): (t, SExp.t) => unit;
  let (>>): (promptPack(t), SExp.t => unit) => unit;
  let (<~): (t, (string, SExp.t)) => unit;
  let count: (t) => int;
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

let isOperator = text => Js.Re.test(text, [%re "/\\+|-|\\*|\\/|<|>/g"]);

let jseval: (string, string, string) => string =
  fun%raw (op, a, b) => "return eval(a+op+b)+''";

let isTrue: string => string = (fun%raw (x, a) => "return x+!!a;")("");

module Make = (Ctx: Context) : {let eval: (Ctx.t, env, SExp.t) => result;} => {
  module StringMap = Map.Make(String);
  let rec evalList = (ctx, env, list) =>
    switch (
      list
      |> List.map(eval(ctx, env))
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
         )
    ) {
    | Result(SExp.List(list)) => Result(SExp.List(list |> List.rev))
    | Result(_) => raise(Invalid)
    | Error(_) as err => err
    }
  and eval = (ctx, env) =>
    fun
    | SExp.List([]) as src => Result(src)
    | SExp.Atom(name) as src when isValid(name) => Result(src)
    | SExp.Atom(name) =>
      switch (List.assoc(name, env)) {
      | data => Result(data)
      | exception Not_found =>
        Error(SExp.List([SExp.Atom("SymbolNotFound"), SExp.Atom(name)]))
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
    | SExp.List([SExp.Atom("debug"), ...next]) =>
      switch (next |> evalList(ctx, env)) {
      | Result(rst) =>
        Ctx.(ctx << rst);
        Result(SExp.empty);
      | Error(_) as err => err
      }
    | SExp.List([SExp.Atom("dump")]) =>
      Result(
        SExp.List(
          env |> List.map(((k, v)) => SExp.List([SExp.Atom(k), v])),
        ),
      )
    | SExp.List([SExp.Atom("clear")]) => {
        ctx |> Ctx.clear;
        Result(SExp.empty);
      }
    | SExp.List([SExp.Atom("let"), SExp.List(vars), ...body]) => {
        let rec loop = prev => (
          fun
          | [] => prev
          | [SExp.List([SExp.Atom(key), value]), ...next] =>
            loop([(key, value), ...prev], next)
          | _ => raise(Invalid)
        );
        switch (loop(env, vars)) {
        | nenv =>
          let rec loop = (
            fun
            | [] => Error(SExp.Atom("InvalidLetBody"))
            | [only] => eval(ctx, nenv, only)
            | [hd, ...tl] => {
                ignore(eval(ctx, nenv, hd));
                loop(tl);
              }
          );
          loop(body);
        | exception Invalid => Error(SExp.Atom("InvalidLet"))
        };
      }
    | SExp.List([SExp.Atom("eval"), ...body]) => body |> evalList(ctx, env)
    | SExp.List([SExp.Atom(sp), a, b]) when isOperator(sp) => {
        let proc = (fn, x) =>
          switch (x |> eval(ctx, env)) {
          | Result(SExp.Atom(xv)) when isValid(xv) => fn(xv)
          | Result(_) => Error(SExp.List([SExp.Atom("FailedToConvert"), a]))
          | err => err
          };
        proc(
          av => proc(bv => Result(SExp.Atom(jseval(sp, av, bv))), b),
          a,
        );
      }
    | SExp.List([SExp.Atom("define"), SExp.Atom(name), body]) =>
      switch (eval(ctx, env, body)) {
      | Result(rst) =>
        Ctx.(ctx <~ (name, rst));
        Result(
          SExp.List([
            SExp.Atom("defined"),
            SExp.List([SExp.Atom("quote"), SExp.Atom(name)]),
            rst,
          ]),
        );
      | err => err
      }
    | SExp.List([
        SExp.Atom("fun"),
        SExp.List(_),
        SExp.List([SExp.Atom("let"), SExp.List(_), ..._]),
      ]) as src =>
      Result(src)
    | SExp.List([SExp.Atom("fun"), SExp.List(params), ...body]) =>
      Result(
        SExp.List([
          SExp.Atom("fun"),
          SExp.List(params),
          SExp.List([
            SExp.Atom("let"),
            SExp.List(
              env |> purgeEnv(ctx |> Ctx.count) |> List.map(((k, v)) => SExp.List([SExp.Atom(k), v])),
            ),
            ...body,
          ]),
        ]),
      )
    | SExp.List([
        SExp.List([SExp.Atom("fun"), SExp.List(params), ...body]) as fn,
        ...real,
      ]) =>
      if (List.length(real) > List.length(params)) {
        Error(SExp.List([SExp.Atom("InvalidCall"), fn]));
      } else {
        let rec loop = prev => (
          fun
          | ([], []) =>
            eval(
              ctx,
              env,
              SExp.List([
                SExp.Atom("let"),
                SExp.List(
                  prev
                  |> List.map(((k, v)) => SExp.List([SExp.Atom(k), v])),
                ),
                ...body,
              ]),
            )
          | (list, []) =>
            Result(
              SExp.List([
                SExp.Atom("fun"),
                SExp.List(list),
                SExp.List([
                  SExp.Atom("let"),
                  SExp.List(
                    prev
                    |> List.map(((k, v)) => SExp.List([SExp.Atom(k), v])),
                  ),
                  ...body,
                ]),
              ]),
            )
          | ([SExp.Atom(name), ...pn], [r, ...rn]) =>
            loop([(name, r), ...prev], (pn, rn))
          | _ => Error(SExp.Atom("InvalidFunction"))
        );
        loop([], (params, real));
      }
    | SExp.List([SExp.List(_) as list, ...next]) =>
      switch (eval(ctx, env, list)) {
      | Result(rst) => eval(ctx, env, SExp.List([rst, ...next]))
      | err => err
      }
    | SExp.List([SExp.Atom(fn), ...next]) when env |> List.mem_assoc(fn) =>
      eval(ctx, env, SExp.List([env |> List.assoc(fn), ...next]))
    | _ => Error(SExp.Atom("NotFound"));
};