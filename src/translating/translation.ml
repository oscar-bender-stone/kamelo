open LP.Syntax
open Common.Type

open Interface.LP_p_term
open Interface.Getter_term

open Symbol
open Axiom

let symbol_to_p_symbol : symbol -> attribute list -> p_command =
  fun s attr_l ->
  let name, qvar_l, _, _ = s in
  Viry.symb_signature :=
    StrMap.add name (sym_curry s) !Viry.symb_signature ;
  let param_l = create_p_params qvar_l in
  let res = create_p_symbol (get_modifier attr_l) name param_l (Some (sym_curry s)) None in
  create_LP_symbol res
