open LP.Syntax
open Common.Type

open Interface.LP_p_term
open Interface.Getter_term

open Symbol
open Axiom

(** Importation *)

(** [import_to_require_open path i] translates a Kore import to a
    "require open" command, with only one path and without position. *)
let import_to_require_open : string list -> import -> p_command = fun path i ->
  let filename = String.lowercase_ascii (fst i)  in
  let path = [create_p_path (path @ [filename])] in
  no_pos (P_require (true, path))

(** Sort *)
let sort_to_p_symbol : sort -> p_command = fun s ->
  let sort_type = get_sort_type s in
  (* sort_signature := StrMap.add s sort_type !sort_signature ; *)
  let res = create_p_symbol [] s [] (Some sort_type) None in
  create_LP_symbol res
  (* modifier = Const ? *)

(** Symbol *)
let symbol_to_p_symbol : symbol -> attribute list -> p_command =
  fun s attr_l ->
  let name, qvar_l, _, _ = s in
  Viry.symb_signature :=
    StrMap.add name (sym_curry s) !Viry.symb_signature ;
  let param_l = create_p_params qvar_l in
  let res = create_p_symbol (get_modifier attr_l) name param_l (Some (sym_curry s)) None in
  create_LP_symbol res
