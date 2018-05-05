exception Imposible;

exception Invalid;

type env = list((string, SExp.t));

let (>=>) = (a, b) => List.rev_append(a, b);

let partitionList = (num, env) => {
  let rec loop = prev =>
    fun
    | (0, list) => (prev, list |> List.rev)
    | (n, [hd, ...tl]) => (n - 1, tl) |> loop([hd, ...prev])
    | _ => raise(Invalid);
  (num, env |> List.rev) |> loop([]);
};

type promptPack('t) =
  | Prompt('t, string);

module type Context = {
  type t;
  let clear: t => unit;
  let (<<): (t, SExp.t) => unit;
  let (>>): (promptPack(t), SExp.t => unit) => unit;
  let (<~): (t, (string, SExp.t)) => unit;
  let (%): (t, string) => SExp.t;
  let has: (t, string) => bool;
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

let isOperator = text =>
  Js.Re.test(text, [%re "/\\+|-|\\*|\\/|<|>|&&|\\|\\|/g"]);

let jseval: (string, string, string) => string =
  fun%raw (op, a, b) => "return eval(a+op+b)+''";

let isTrue: (string, 'a, 'a) => 'a  = (fun%raw (x, a, b) => "return eval(x) ? a : b;");

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
      if (List.mem_assoc(name, env)) {
        Result(List.assoc(name, env));
      } else if (Ctx.has(ctx, name)) {
        Result(Ctx.(ctx % name));
      } else {
        Error(SExp.List([SExp.Atom("SymbolNotFound"), SExp.Atom(name)]));
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
            switch (eval(ctx, env, value)) {
            | Result(nval) => loop([(key, nval), ...prev], next)
            | _ => raise(Invalid)
            }
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
          | Error(err) =>
            Error(SExp.List([SExp.Atom("ErrorInsideOp"), err]))
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
    | SExp.List([SExp.Atom("if"), cond, itrue, ifalse]) =>
      switch (eval(ctx, env, cond)) {
      | Result(SExp.Atom(rst)) when isValid(rst) => isTrue(rst, itrue, ifalse) |> eval(ctx, env)
      | _ => Error(SExp.List([SExp.Atom("InvalidCond"), cond]))
      }
    | SExp.List([
        SExp.Atom("fun"),
        SExp.List(_),
        SExp.List([SExp.Atom("let"), SExp.List(_), ..._]),
      ]) as src =>
      Result(src)
    | SExp.List([
        SExp.Atom("defun"),
        SExp.Atom(name),
        SExp.List(_) as params,
        ...body,
      ]) =>
      eval(
        ctx,
        env,
        SExp.List([
          SExp.Atom("define"),
          SExp.Atom(name),
          SExp.List([SExp.Atom("fun"), params, ...body]),
        ]),
      )
    | SExp.List([SExp.Atom("fun"), SExp.List(params), ...body]) =>
      Result(
        SExp.List([
          SExp.Atom("fun"),
          SExp.List(params),
          SExp.List([
            SExp.Atom("let"),
            SExp.List(
              env |> List.map(((k, v)) => SExp.List([SExp.Atom(k), v])),
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
        let (prev, next) =
          real |> partitionList(List.length(real) - List.length(params));
        eval(ctx, env, SExp.List([SExp.List([fn, ...next]), ...prev]));
      } else {
        let mkCore = prev =>
          switch (body) {
          | [SExp.List([SExp.Atom("let"), SExp.List(list), ...next])] =>
            SExp.List([
              SExp.Atom("let"),
              SExp.List(
                prev
                |> List.map(((k, v)) => SExp.List([SExp.Atom(k), v]))
                >=> list,
              ),
              ...next,
            ])
          | _ =>
            SExp.List([
              SExp.Atom("let"),
              SExp.List(
                prev |> List.map(((k, v)) => SExp.List([SExp.Atom(k), v])),
              ),
              ...body,
            ])
          };
        let rec loop = prev => (
          fun
          | ([], []) => eval(ctx, env, prev |> mkCore)
          | (list, []) =>
            Result(
              SExp.List([
                SExp.Atom("fun"),
                SExp.List(list),
                prev |> mkCore,
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
    | SExp.List([SExp.Atom(fn), ...next]) when Ctx.has(ctx, fn) =>
      eval(ctx, env, SExp.List([Ctx.(ctx % fn), ...next]))
    | unknown => Error(SExp.List([SExp.Atom("UnknownCommand"), unknown]));
};