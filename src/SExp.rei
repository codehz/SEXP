type t =
  | Atom(string)
  | List(list(t));

let empty: t;

let toString: t => string;

let parse: string => t;