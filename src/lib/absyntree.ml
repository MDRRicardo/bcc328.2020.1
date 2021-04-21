(* Convert abstract syntax trees to generic trees of list of string *)

open Absyn

(* Helper functions *)

let name = Symbol.name
let map = List.map
let sprintf = Format.sprintf

(* Concatenate the lines of text in the node *)
let flat_nodes tree =
  Tree.map (String.concat "\n") tree

(* Build a singleton tree from a string *)
let mktr s = Tree.mkt [s]

(* Convert a symbol to a general tree *)
let tree_of_symbol s = mktr (Symbol.name s) []

(* Convert a binary operator to a string *)
let stringfy_operator op =
  match op with
  | Plus -> "+"
  | LT -> "<"

(* Convert an expression to a generic tree *)
let rec tree_of_exp exp =
  match exp with
  | IntExp x -> mktr (sprintf "IntExp %i" x) []
  | VarExp x -> mktr (sprintf "VarExp %s" (name x)) []
  | OpExp (op, l, r) -> mktr (sprintf "OpExp %s" (stringfy_operator op)) [tree_of_lexp l; tree_of_lexp r]
  | IfExp (t, x, y) -> mktr "IfExp" [tree_of_lexp t; tree_of_lexp x; tree_of_lexp y]
  | CallExp (f, a) -> mktr (sprintf "CallExp %s" (name f)) (map tree_of_lexp a)
  | LetExp (x, i, e) -> mktr (sprintf "LetExp %s" (name x)) [tree_of_lexp i; tree_of_lexp e]

and tree_of_fundec (typeid, params, body) =
  mktr
    "Fun"
    [ tree_of_typeid typeid;
      mktr "Formals" (List.map tree_of_typeid params);
      tree_of_lexp body
    ]

and tree_of_program fundecs =
  mktr "Program" (List.map tree_of_lfundec fundecs)

and tree_of_typeid (type_, id) =
  mktr (sprintf "%s:%s" (name id) (show_type_ type_)) []

(* Convert an anotated expression to a generic tree *)
and tree_of_lexp (_, x) = tree_of_exp x

and tree_of_lfundec (_, x) = tree_of_fundec x

and tree_of_lsymbol (_, x) = tree_of_symbol x
