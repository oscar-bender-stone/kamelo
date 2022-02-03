open Common.Type
open Common.Color

open LP.Syntax
open Interface.LP_p_term
open Interface.K_prelude

open Axiom
open Symbol

let def_to_p_term : def -> p_term = fun d ->
  match d with
  | A ax    ->
     begin
       match ax with
       | And(_,a1,a2) ->
          if is_conditional_rule a1 then
            (* raise (Axiom.ConditionalRule "Conditional rewriting rule not supported yet.")*)
            p_TYPE
          else
            (try curry_ident a2
             with KComputation _ ->
               Format.printf (yel "WARNING: K computation found\n") ; p_TYPE)
       | Predicate p -> (match p with Sym _ -> p_TYPE | Var _ -> p_TYPE)
       |  _ -> failwith "In LHS: Not yet implemented"
     end
  | D (n,_) -> create_ident n

let param_to_p_term p = match p with S s -> get_type s | Q _ -> p_TYPE

(** [create_p_params_expl l] creates explicit parameters, which have the current given type,
    without position. Note: p_params = p_ident option list * p_term option * bool. *)
let create_p_params_expl : (name * param) list -> p_params list = fun s_l ->
  let is_implicit = false in
  let f (n,p) = ([Some (create_p_ident n)], Some (param_to_p_term p), is_implicit)  in
  List.map f s_l

(** [create_p_params s_l] creates implicit parameters, which have the type _SORTK,
    without position. Note: p_params = p_ident option list * p_term option * bool. *)
let create_p_params : string list -> p_params list = fun s_l ->
  match s_l with
  | []   -> []
  | _::_ ->
     let unique_name s = Some (no_pos s)  in
     let typ = Some p_SORTK in
     let is_implicit = true in
     [ List.map unique_name s_l, typ, is_implicit ]

let alias_to_definition : alias -> p_command = fun al ->
  let (name, qv_l, _, p), (name_bis, qv_l_bis, expl_l, def) = al in
  let _ =
    if not(name = name_bis) (* && qv_l = qv_l_bis) *) then qv_l else qv_l_bis
  in
  (* STEP 0: Get the signature *)

  (* STEP 1: Get the definition of the symbol *)
  let body = def_to_p_term def in
  (* STEP 2: Build the p_symbol *)
  let p_l = create_p_params qv_l @ (create_p_params_expl expl_l) in
  no_pos (P_symbol (create_p_symbol [] name p_l (Some (param_to_p_term p)) (Some body)))
