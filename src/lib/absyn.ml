(* absyn.ml *)

type symbol = Symbol.symbol
  [@@deriving show]

type 'a loc = 'a Location.loc
  [@@deriving show]

type operator =
  | Plus
  | LT
  [@@deriving show]

type exp =
  | IntExp of int
  | VarExp of symbol
  | OpExp of operator * lexp * lexp
  | IfExp of lexp * lexp * lexp
  | CallExp of symbol * lexp list
  | LetExp of symbol * lexp * lexp
  [@@deriving show]

and program = lfundec list

and fundec = (type_ * symbol) * (type_ * symbol) list * lexp
  [@@deriving show]

and type_ =
  | Int
  | Bool
  [@@deriving show]

and lexp = exp loc
  [@@deriving show]

and lfundec = fundec loc
  [@@deriving show]
