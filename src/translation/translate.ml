
open LP.Syntax
open Common.Type
open Common.Getter
open Interface.LP_p_term
open Interface.K_prelude

open Symbol
open Axiom
open Alias

(** Importation *)

(** [import_to_require_open path i] translates a Kore import to a "require open"
    command, with only one path and without position. *)
let import_to_require_open : string list -> import -> p_command = fun path i ->
  let filename = String.lowercase_ascii (fst i)  in
  let path = [create_p_path (path @ [filename])] in
  no_pos (P_require (true, path))

(** To store the type of each symbol *)
let symb_signature : p_term StrMap.t ref = ref StrMap.empty

(** Sort *)
let get_sort_type : sort -> p_term = fun s ->
  if s = _SORTK then p_TYPE else p_SORTK

let sort_to_p_symbol : sort -> p_command = fun s ->
  let sort_type = get_sort_type s in
  sort_signature := StrMap.add s sort_type !sort_signature ;
  let res = create_p_symbol [] s [] (Some sort_type) None in
  no_pos (P_symbol res)
  (* modifier = Const ? *)

(** Symbol *)
let symbol_to_p_symbol : symbol -> attribute list -> p_command = fun s attr_l ->
  let name, qvar_l, _, _ = s in
  symb_signature := StrMap.add name (sym_curry s) !symb_signature ;
  let param_l = create_p_params qvar_l in
  let res = create_p_symbol (get_modifier attr_l) name param_l (Some (sym_curry s)) None in
  no_pos (P_symbol res)

(** Inductive type *)
let induc_to_p_inductive : sort * symbol list -> p_inductive = fun (sort, s_l) ->
  (* p_inductive_aux = p_ident * p_term * (p_ident * p_term) list *)
  let f s = (create_p_ident (get_name s), sym_curry s) in
  no_pos (create_p_ident sort, p_TYPE, List.map f s_l)

(** [create_inductive_type i] creates non-mutual inductive type without parameter
    and position. *)
let create_inductive_type : sort * symbol list -> p_command = fun i ->
  no_pos (P_inductive ([], [], [induc_to_p_inductive i]))

(** Alias *)
let unconditional_rule_to_p_rule : alias -> axiom -> p_command = fun al ax ->
  no_pos (P_rules [create_rewriting_rule al ax])

(** Axiom *)
let equality_axiom_to_p_rule : axiom -> p_command = fun ax ->
  no_pos (P_rules [of_equality_axiom ax])
