
open LP.Syntax
open Common.Type
open Common.Getter

open Common.Xlib_OCaml
open Interface.Output
open Mecanism.Iterator

open Translating.Sort
open Translating.Symbol
open Translating.Axiom
open Translating.Eval_strategy
open Translating.Viry

let main cd : (* TODO fix heterogenous signature *)
      kommand list -> p_term StrMap.t ->
      (p_command list * p_command list * p_symbol list * p_rule list) *
        p_term StrMap.t =
  fun kommand_l init_sign ->
  (* STEP 1: From K commands to CTRS rules (and partial printing). *)
  let f_sort _ acc sign s =
    let new_s = sort_to_p_symbol (pp s) in
    let sort_l, sym_l, ctrs_l = acc in (new_s::sort_l, sym_l, ctrs_l), sign
  in
  let f_symbol attr_l acc sign s =
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
    let sort_l, sym_l, ctrs_l = acc in
    (sort_l, new_s::sym_l, ctrs_l), StrMap.add name (sym_curry s) sign
  in
  let propagation = fun _ acc sign _ -> (acc, sign) in
  let f_subsort (* :
        attribute list -> p_command list * p_command list * ctrs_rule list ->
        quant_var list * axiom -> p_command list * p_command list * ctrs_rule list *) =
    fun _ acc sign (_, ax) -> collect_subsort_data ax ; (acc, sign)
  in
  let trans_implies =
    fun _ acc sign (_, ax) ->
    let sort_l, sym_l, ctrs_l = acc in
    (sort_l, sym_l, (of_implies_axiom ax)::ctrs_l), sign
  in
  (* let update_data : kommand -> unit = fun (c, _) ->
    match c with
    | Symbol (name, _, _, _ as s) ->
       symb_signature := StrMap.add name (sym_curry s) !symb_signature
    | _ -> ()
  in *)
  let (sort_l, sym_l, ctrs_r_l), sign =
    kommand_iter_without_alias cd kommand_l ([], [], []) init_sign
    f_sort propagation f_symbol propagation propagation
    ((fun attr_l (sort_l, sym_l, r_l) sign al ax  ->
      (sort_l, sym_l, trans_heating_rule  attr_l r_l al ax), sign),
     (fun attr_l (sort_l, sym_l, r_l) sign al ax  ->
      (sort_l, sym_l, trans_cooling_rule  attr_l r_l al ax), sign),
     (fun attr_l (sort_l, sym_l, r_l) sign al ax  ->
      (sort_l, sym_l, trans_semantic_rule attr_l r_l al ax), sign))
    propagation (f_subsort, propagation)
    (propagation, propagation, propagation, propagation)
    propagation propagation
    (propagation, trans_implies, trans_implies, trans_implies,
     propagation, trans_implies, trans_implies)
    (fun () -> ())
  in
  (* STEP 2: From CTRS rules to TRS rules and symbols. *)
  let sym_add_l, r_l = viry_encoding ctrs_r_l sign in
  (sort_l, sym_l, sym_add_l, r_l), sign
