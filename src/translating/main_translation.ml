
open LP.Syntax
open Common.Type
open Common.Getter
open Interface.LP_p_term
open Interface.K_prelude
open Interface.Getter_term

open Interface.Output

open Mecanism.Iterator_plus_plus

open Symbol
open Axiom

open Eval_strategy

(** Importation *)

(** [import_to_require_open path i] translates a Kore import to a
    "require open" command, with only one path and without position. *)
let import_to_require_open : string list -> import -> p_command = fun path i ->
  let filename = String.lowercase_ascii (fst i)  in
  let path = [create_p_path (path @ [filename])] in
  no_pos (P_require (true, path))

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
let symbol_to_p_symbol : symbol -> attribute list -> p_command =
  fun s attr_l ->
  let name, qvar_l, _, _ = s in
  Viry.symb_signature :=
    StrMap.add name (sym_curry s) !Viry.symb_signature ;
  let param_l = create_p_params qvar_l in
  let res = create_p_symbol (get_modifier attr_l) name param_l (Some (sym_curry s)) None in
  no_pos (P_symbol res)

(** Inductive type *)
let induc_to_p_inductive : sort * symbol list -> p_inductive =
  fun (sort, s_l) ->
  (* p_inductive_aux = p_ident * p_term * (p_ident * p_term) list *)
  let f s = (create_p_ident (get_name s), sym_curry s) in
  no_pos (create_p_ident sort, p_TYPE, List.map f s_l)

(** [create_inductive_type i] creates non-mutual inductive type
    without parameter and position. *)
let create_inductive_type : sort * symbol list -> p_command = fun i ->
  no_pos (P_inductive ([], [], [induc_to_p_inductive i]))

(** Alias *)
let unconditional_rule_to_p_rule : alias -> axiom -> p_command =
  fun al ax -> no_pos (P_rules [create_rewriting_rule al ax])

(** Axiom *)
let equality_axiom_to_p_rule : axiom -> p_command = fun ax ->
  no_pos (P_rules [of_equality_axiom ax])


let encoding_with_Viry cd : (* TODO fix heterogenous signature *)
      kommand list -> p_command list * p_command list * p_symbol list * p_rule list =
  fun kommand_l ->
  (* STEP 1: From K commands to CTRS rules (and partial printing). *)
  let f_sort _ acc s =
    let new_s = sort_to_p_symbol (pp s) in
    let sort_l, sym_l, ctrs_l = acc in (new_s::sort_l, sym_l, ctrs_l)
  in
  let f_symbol attr_l acc s =
    (match is_constructor s attr_l with
     | Some sort ->
        let f new_v old_v = match old_v with
          | None   -> Some [new_v]
          | Some q -> Some (new_v::q)
        in
        data_induc := Induc.update sort (f s) !data_induc
     | None -> () ) ;
    let name, qv_l, p_l, p = s in (* TODO delete *)
    let s = (pp name, qv_l, p_l, p) in
    let new_s = symbol_to_p_symbol s attr_l in
    let sort_l, sym_l, ctrs_l = acc in (sort_l, new_s::sym_l, ctrs_l)
  in
  let propagation = fun _ acc _ -> acc in
  let f_subsort :
        attribute list -> p_command list * p_command list * ctrs_rule list ->
        quant_var list * axiom -> p_command list * p_command list * ctrs_rule list =
    fun _ acc (_, ax) -> collect_subsort_data ax ; acc
  in
  let trans_implies =
    fun _ acc (_, ax) ->
    let sort_l, sym_l, ctrs_l = acc in sort_l, sym_l, (of_implies_axiom ax)::ctrs_l
  in
  let sort_l, sym_l, ctrs_r_l =
    kommand_iter_without_alias cd kommand_l ([], [], [])
    f_sort propagation f_symbol propagation propagation
    ((fun attr_l (sort_l, sym_l, r_l) al ax  ->
      sort_l, sym_l, trans_heating_rule  attr_l r_l al ax),
     (fun attr_l (sort_l, sym_l, r_l) al ax  ->
       sort_l, sym_l, trans_cooling_rule  attr_l r_l al ax),
     (fun attr_l (sort_l, sym_l, r_l) al ax  ->
       sort_l, sym_l, trans_semantic_rule attr_l r_l al ax))
    propagation (f_subsort, propagation)
    (propagation, propagation, propagation, propagation)
    propagation propagation
    (propagation, trans_implies, trans_implies, trans_implies,
     propagation, trans_implies, trans_implies)
    (fun () -> ())
  in
  (* STEP 2: From CTRS rules to TRS rules and symbols. *)
  let sym_add_l, r_l = Viry.viry_encoding ctrs_r_l in
  (sort_l, sym_l, sym_add_l, r_l)
