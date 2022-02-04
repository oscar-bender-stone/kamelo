open Common.Type
open Common.Getter
open Common.Error

open LP.Syntax
open Interface.LP_p_term
open Interface.Getter_term

open Axiom

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
               wrn_msg _STDOUT "WARNING: K computation found" ; p_TYPE)
       | Predicate p -> (match p with Sym _ -> p_TYPE | Var _ -> p_TYPE)
       |  _ -> failwith "In LHS: Not yet implemented"
     end
  | D (n,_) -> create_ident n


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
