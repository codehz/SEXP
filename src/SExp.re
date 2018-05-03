type t =
  | Atom(string)
  | List(list(t));

let empty = List([]);

let rec toString =
  fun
  | Atom(str) => str
  | List(list) => {
      let rec join = (
        fun
        | [] => ""
        | [only] => toString(only)
        | [head, ...tail] => toString(head) ++ " " ++ join(tail)
      );
      "(" ++ join(list) ++ ")";
    };

let to_list = s => {
  let rec loop = (acc, i) =>
    if (i == (-1)) {
      acc;
    } else {
      loop([s.[i], ...acc], pred(i));
    };
  loop([], String.length(s) - 1);
};

let iteri = (f, l) => {
  let rec loop = i =>
    fun
    | [] => ()
    | [x, ...xs] => {
        f(i, x);
        loop(succ(i), xs);
      };
  loop(0, l);
};

let of_list = xs => {
  let l = List.length(xs);
  let s = Bytes.create(l);
  iteri((i, c) => Bytes.set(s, i, c), xs);
  Bytes.unsafe_to_string(s);
};

type token =
  | Bracket(bool)
  | Spaces
  | Entity(string);

type charCategory =
  | LeftBracket
  | RightBracket
  | Space
  | Normal(char);

let getCharCategory =
  fun
  | '(' => LeftBracket
  | ')' => RightBracket
  | ' '
  | '\t'
  | '\r'
  | '\n' => Space
  | ch => Normal(ch);

let getCCList = list => list |> List.map(getCharCategory);

let tokenizer = input =>
  input
  |> to_list
  |> getCCList
  |> (
    list => {
      let rec proc =
        fun
        | [LeftBracket, ...next] => [Bracket(true), ...proc(next)]
        | [RightBracket, ...next] => [Bracket(false), ...proc(next)]
        | [Space, ...next] => {
            let rec eat = (
              fun
              | [Space, ...next] => eat(next)
              | list => list
            );
            [Spaces, ...proc(eat(next))];
          }
        | [Normal(ch), ...next] => {
            let rec feed = text => (
              fun
              | [Normal(ch), ...next] => feed([ch, ...text], next)
              | next => (text |> List.rev, next)
            );
            let (text, tail) = feed([ch], next);
            [Entity(text |> of_list), ...proc(tail)];
          }
        | [] => [];
      proc(list);
    }
  );

exception ParseFailed;

let parseList = input => {
  let rec proc = prev =>
    fun
    | [Spaces, ...next] => proc(prev, next)
    | [Entity(str), ...next] => proc([Atom(str), ...prev], next)
    | [Bracket(true), ...next] => {
        let (nlist, nnext) = proc([], next);
        proc([List(nlist), ...prev], nnext);
      }
    | [Bracket(false), ...next] => (prev |> List.rev, next)
    | _ => raise(ParseFailed);
  proc([], input);
};

let parse = input => {
  let rec valid = (
    fun
    | [] => []
    | [Bracket(true), ...next] => next
    | [_, ...tl] => valid(tl)
  );
  let (list, _) = input |> String.trim |> tokenizer |> valid |> parseList;
  List(list);
};