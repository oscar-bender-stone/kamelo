
open LP_interface.Syntax
open Common.Type
open LP_interface.LP_p_term

open Symbol
open Axiom

(** Importation *)

(** [import_to_require_open path i] translates a Kore import to a "require open"
    command, with only one path and without position. *)
let import_to_require_open : string list -> import -> p_command = fun path i ->
  let filename = String.lowercase_ascii (fst i)  in
  let path = [create_p_path (path @ [filename])] in
  no_pos (P_require (true, path))

(** [create_p_params s_l] creates implicit parameters, which have the type _SORTK,
    without position. Note: p_params = p_ident option list * p_term option * bool. *)
let create_p_params : string list -> p_params list = fun s_l ->
  match s_l with
  | []   -> []
  | _::_ ->
     let unique_name s = Some (no_pos s)  in
     let typ = Some (create_ident _SORTK) in
     let is_implicit = true in
     [ List.map unique_name s_l, typ, is_implicit ]

(** Sort *)
let get_sort_type : sort -> p_term = fun s ->
  if s = _SORTK then _TYPE else create_ident _SORTK

let sort_to_p_symbol : sort -> p_command = fun s ->
  let sort_type = get_sort_type s in
  let res = create_p_symbol [] s [] (Some sort_type) None in
  no_pos (P_symbol res)
  (* modifier = Const ? *)

(** Symbol *)
let symbol_to_p_symbol : symbol -> attribute list -> p_command = fun s attr_l ->
  let name, qvar_l, _, _ = s in
  let param_l = create_p_params qvar_l in
  let res = create_p_symbol (get_modifier attr_l) name param_l (Some (sym_curry s)) None in
  no_pos (P_symbol res)

(** [create_symbol sym] creates a symbol without position. *)
let create_symbol : p_symbol -> p_command = fun sym ->
   no_pos (P_symbol sym)

(** Inductive type *)
let induc_to_p_inductive : sort * symbol list -> p_inductive = fun (sort, s_l) ->
  (* p_inductive_aux = p_ident * p_term * (p_ident * p_term) list *)
  let f s = (create_p_ident (get_name s), sym_curry s) in
  no_pos (create_p_ident sort, _TYPE, List.map f s_l)

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
