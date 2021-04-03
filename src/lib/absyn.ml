(* absyn.ml *)

type symbol = Symbol.symbol
  [@@deriving show]

type 'a loc = 'a Location.loc
  [@@deriving show]

type operator =
  | Plus
  | LT
  | EQ
  [@@deriving show]

type exp =
  | IntExp  of int
  | IdExp   of symbol
  | IdFunctionExp of symbol * lexp list
  | IfExp   of lexp * lexp * lexp
  | LetExp  of symbol * lexp * lexp
  | OpExp   of operator * lexp * lexp
  [@@deriving show]

and exps = (lexp) list
  [@@deriving show]

and funs = (lfundec) list
  [@@deriving show]

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
