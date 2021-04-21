(* interpreter.ml *)
(* here im waiting lexp -> already has location*)
let rec check_exp(exp, vtable, ftable) =
  match exp with
  | (_, Absyn.IntExp _) -> Absyn.Int
  | (_, Absyn.VarExp x) ->
    (match Symbol.look x vtable with
    | Some var -> var
    | _ -> Error.error (Location.loc exp) "debug var(id)" Absyn.Int)
  | (_, Absyn.OpExp (Absyn.Plus, left, right)) ->
      let t1 = check_exp(left, vtable, ftable) in
      let t2 = check_exp(right, vtable, ftable) in
      if t1 == Absyn.Int && t2 == Absyn.Int
        then Absyn.Int
      else
        Error.error (Location.loc exp) "debug plus"
        Absyn.Int
  | (_, Absyn.OpExp (Absyn.LT, left, right)) ->
      let t1 = check_exp(left, vtable, ftable) in
      let t2 = check_exp(right, vtable, ftable) in
      if t1 == t2
        then Absyn.Bool
      else
        Error.error (Location.loc exp) "debug lt"
        Absyn.Bool
  | (_, Absyn.IfExp (if', then', else')) ->
      let t1 = check_exp(if', vtable, ftable) in
      let t2 = check_exp(then', vtable, ftable) in
      let t3 = check_exp(else', vtable, ftable) in
      if t1 == Absyn.Bool && t2 == t3
        then t2
      else
        Error.error (Location.loc exp) "debug if"
        t2
  | (_, Absyn.CallExp (id, exps)) ->
      let t = Symbol.look id ftable in
      (match t with
        | Some exp_list' ->
          let param_length = List.length exp_list' in
          let verify_exps = check_exps(exps, vtable, ftable) in
          let verified_exps_length = List.length verify_exps in
          let t0 = Symbol.look id vtable in
          if cmp verify_exps exp_list' && verified_exps_length == param_length
            then (match t0 with
                  | Some var -> var
                  | None -> Error.error (Location.loc exp) "debug var(id)")
          else
            Error.error (Location.loc exp) "debug callexp"
            t0
        | None -> Error.error (Location.loc exp) "debug callexp unbound"
                  Absyn.Int)
  | (_, Absyn.LetExp (id, id_exp, in_exp)) ->
      let t1 = check_exp(id_exp, vtable, ftable) in
      let new_vtable = Symbol.enter id t1 vtable in
        check_exp(in_exp, new_vtable, ftable)

(* not loc *)
and check_exps(exps, vtable, ftable) =
  match exps with
  | [exp] -> [check_exp(exp, vtable, ftable)]
  | exp :: tail -> check_exp(exp, vtable, ftable) :: check_exps(tail, vtable, ftable)
  | [] -> []

and cmp list1 list2 =
  match (list1, list2) with
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false
  | [a], [b] -> a == b
  | (a :: tail1), (b :: tail2) -> a == b && cmp tail1 tail2


(* type checking a function declaration
   code according to page 121 of Introduction to Compiler Design*)
(* checkfun ( fun, table ) -> with loc *)
let rec check_fun((loc, typeid, typeids, exp), ftable) =
  let (_, t0) = get_type_id(typeid) in
  let vtable = check_type_ids(typeids) in
  let t1 = check_exp(exp, vtable, ftable) in
  if t0 != t1
    then Error.error loc "debug check funs"

(* not loc *)
and get_type_id(type', id) =
  match type' with
  | Absyn.Int -> (id, Absyn.Int)
  | Absyn.Bool -> (id, Absyn.Bool)

(* not loc *)
and check_type_ids(typeids) =
  match typeids with
  | [] -> Symbol.empty (* throw error *)
  | [typeid] -> let (x, t) = get_type_id(typeid) in
                Symbol.enter x t Symbol.empty
  | typeid :: tail -> let (x, t) = get_type_id(typeid) in
                      let vtable = check_type_ids(tail) in
                      match Symbol.look x vtable with
                      | Some _ -> Symbol.empty (* throw error *)
                      | None -> Symbol.enter x t vtable

(* type checking a program
   code according to page 122 of Introduction to Compiler Design *)
(* not loc *)
let rec check_funs(funs, ftable) =
  match funs with
  | [] -> ()
  | [fun'] -> check_fun(fun', ftable)
  | fun' :: tail -> check_fun(fun', ftable); check_funs(tail, ftable)

let rec get_types(type_ids) =
  match type_ids with
  | [] -> []
  | [type_id] -> let (_, t) = get_type_id(type_id) in [t]
  | type_id :: tail -> let (_, t) = get_type_id(type_id) in
                       let tail_ = get_types(tail) in t :: tail_

let get_fun(fun') =
  match fun' with
  | (loc, typeId, type_ids, _) -> let (f, t0) = get_type_id(typeId) in
                             let types = get_types(type_ids) in
                             (f, types, t0)

let rec get_funs(funs) =
  match funs with
  | [] -> Symbol.empty
  | [fun'] -> let (f, params, return) = get_fun(fun') in
              Symbol.enter f params Symbol.empty
  | fun' :: tail -> let (f, params, return) = get_fun(fun') in
                    let ftable = get_funs(tail) in
                    match Symbol.look f ftable with
                    | None -> Symbol.enter f params ftable
                    | Some _ -> Symbol.empty   (*Error.error (Location.loc fun') "debug funs" ftable*)

let check_program(program) =
  let ftable = get_funs(program) in
  check_funs(program, ftable);
