
open LP.Syntax
open Common.Type
open Common.Error
open Common.Getter
open Common.Xlib_OCaml

open Interface.Signature
open Interface.Output

open Mecanism.Kommand_iterator

open TransSem.Sort
open TransSem.Symbol
open TransSem.Axiom
open TransSem.Eval_strategy
open TransSem.Viry

let main_without_lib cd : (* TODO fix heterogenous signature *)
      kommand list -> signature ->
      (p_command list * p_command list * p_symbol list * p_symbol list * p_rule list) *
        signature =
  fun kommand_l init_sign ->
  (* STEP 1: From K commands to CTRS rules. *)
  let f_sort _ acc sign s =
    let new_s = sort_to_p_symbol (pp s) in
    let sort_l, sym_l, ctrs_l = acc in (new_s::sort_l, sym_l, ctrs_l), sign
  in
  let f_symbol (attr_l,_) acc sign s =
    let sign = match is_constructor s attr_l with
      | Some sort ->
         { sign with inductive = add_update_induc sort s sign.inductive }
      | None -> sign
    in
    let name, qv_l, p_l, p = s in (* TODO delete *)
    let s = (pp name, qv_l, p_l, p) in
    let new_s = symbol_to_p_symbol s attr_l in
    let sort_l, sym_l, ctrs_l = acc in
    let new_typing = StrMap.add (pp name) (curry_symbol s) sign.typing in
    (sort_l, new_s::sym_l, ctrs_l), { sign with typing = new_typing }
  in
  let propagation = fun _ acc sign _ -> (acc, sign) in
  let f_subsort :
        data -> p_command list * p_command list * ctrs_rule list ->
        signature -> quant_var list * axiom ->
        (p_command list * p_command list * ctrs_rule list) * signature =
    fun _ acc sign (_, ax) -> (acc, collect_subsort_data ax sign)
  in
  let trans_implies =
    fun (_, pos) acc sign (_, ax) ->
    let sort_l, sym_l, ctrs_l = acc in
    try ((sort_l, sym_l, (of_implies_axiom ax)::ctrs_l), sign)
    with KaMeLoError(t, fileN, funcN, msg) -> wrn_no_translation (t, fileN, funcN, msg) pos ; (acc, sign)
  in
  let (sort_l, sym_l, ctrs_r_l), sign =
    kommand_iter_without_alias cd kommand_l ([], [], []) init_sign
    f_sort propagation f_symbol propagation propagation
    ((fun data (sort_l, sym_l, r_l) sign al ax  ->
        (sort_l, sym_l, trans_cooling_rule  data r_l sign al ax), sign),
     (fun data (sort_l, sym_l, r_l) sign al ax  ->
        (sort_l, sym_l, trans_heating_rule  data r_l sign al ax), sign),
     (fun data (sort_l, sym_l, r_l) sign al ax  ->
        (sort_l, sym_l, trans_semantic_rule data r_l sign al ax), sign))
    propagation (f_subsort, propagation)
    (propagation, propagation, propagation, propagation, propagation)
    propagation propagation
    (propagation, trans_implies, trans_implies, trans_implies,
     propagation, trans_implies, trans_implies)
    (fun _ -> raise (KaMeLoError (InternalError, "With_Viry_encoding", "main_without_lib", "Claim in Viry.")) )
    (fun () -> ())
  in
  (* STEP 2: From CTRS rules to TRS rules and symbols. *)
  let flat_sym_add_l, (sym_add_l, r_l) = viry_encoding ctrs_r_l sign in
  if List.length ctrs_r_l > List.length r_l
  then raise (KaMeLoError (InternalError, "With_Viry_encoding", "main_without_lib", "Some rewriting rules disappear."))
  else (sort_l, sym_l, flat_sym_add_l, sym_add_l, r_l), sign




let tmp : string list ref = ref []


let main_with_lib cd : (* TODO fix heterogenous signature *)
      kommand list -> signature ->
      (p_command list * p_command list * p_symbol list * p_symbol list * p_rule list) *
        signature =
  fun kommand_l init_sign ->
  (* STEP 1: From K commands to CTRS rules. *)
  let f_sort _ acc sign s =
    let new_s = sort_to_p_symbol (pp s) in
    let sort_l, sym_l, ctrs_l = acc in (new_s::sort_l, sym_l, ctrs_l), sign
  in
  let f_symbol (attr_l,_) acc sign s =
    let sign = match is_constructor s attr_l with
      | Some sort ->
         { sign with inductive = add_update_induc sort s sign.inductive }
      | None -> sign
    in
    let name, qv_l, p_l, p = s in (* TODO delete *)
    let s = (pp name, qv_l, p_l, p) in
    let new_s = symbol_to_p_symbol s attr_l in
    let sort_l, sym_l, ctrs_l = acc in
    let new_typing = StrMap.add (pp name) (curry_symbol s) sign.typing in
    (sort_l, new_s::sym_l, ctrs_l), { sign with typing = new_typing }
  in
  let propagation = fun _ acc sign _ -> (acc, sign) in
  let propagation_and_get_data = fun _ acc sign (n,_,_,_) -> tmp:=n::!tmp ; (acc, sign) in
  let f_subsort :
        data -> p_command list * p_command list * ctrs_rule list ->
        signature -> quant_var list * axiom ->
        (p_command list * p_command list * ctrs_rule list) * signature =
    fun _ acc sign (_, ax) -> (acc, collect_subsort_data ax sign)
  in
  let trans_implies =
    fun (_, pos) acc sign (_, ax) ->
    let sort_l, sym_l, ctrs_l = acc in
    try ((sort_l, sym_l, (of_implies_axiom ax)::ctrs_l), sign)
    with KaMeLoError(t, fileN, funcN, msg) -> wrn_no_translation (t, fileN, funcN, msg) pos ; (acc, sign)
  in
  let trans_implies_cleaning =
    fun (_, pos) acc sign (_, ax) ->
    let sort_l, sym_l, ctrs_l = acc in
    let name = match ax with
    | Implies(_, Top _,                                       Equals(_, Predicate(Sym(name,_,_)), And(_, _, Top _)))
    | Implies(_, And(_,Top _, _),                            Equals(_, Predicate(Sym(name,_,_)), And(_, _, Top _)))
    | Implies(_, And(_, Equals(_, _, Dom_val(_,"true")), _), Equals(_, Predicate(Sym(name,_,_)), And(_, _, Top _)))
    | Implies(_, Equals(_, _, Dom_val(_,"true")),             Equals(_, Predicate(Sym(name,_,_)), And(_, _, Top _)))
    | Implies (_, And(_, Not(_, Or(_, And(_, Top _, And(_, In(_,(_,_),_), Top _)), Bottom _)), _), Equals(_, Predicate(Sym(name,_,_)), And(_, _, Top _))) ->
     name
    | _ -> raise (KaMeLoError (NotYetImplemented, "Axiom", "main_with_lib", "Case root."))
    in

    if not (!Printing.Meta_printer.lib && List.mem name !tmp) then
      (try ((sort_l, sym_l, (of_implies_axiom ax)::ctrs_l), sign)
       with KaMeLoError(t, fileN, funcN, msg) -> wrn_no_translation (t, fileN, funcN, msg) pos ; (acc, sign))
    else (acc, sign)
  in
  let (sort_l, sym_l, ctrs_r_l), sign =
    kommand_iter_without_alias cd kommand_l ([], [], []) init_sign
    f_sort propagation f_symbol propagation_and_get_data propagation
    ((fun data (sort_l, sym_l, r_l) sign al ax  ->
        (sort_l, sym_l, trans_cooling_rule  data r_l sign al ax), sign),
     (fun data (sort_l, sym_l, r_l) sign al ax  ->
        (sort_l, sym_l, trans_heating_rule  data r_l sign al ax), sign),
     (fun data (sort_l, sym_l, r_l) sign al ax  ->
        (sort_l, sym_l, trans_semantic_rule data r_l sign al ax), sign))
    propagation (f_subsort, propagation)
    (propagation, propagation, propagation, propagation, propagation)
    propagation propagation
    (propagation, trans_implies, propagation, propagation,
     propagation, trans_implies_cleaning, trans_implies_cleaning)
    (fun _ -> raise (KaMeLoError (InternalError, "With_Viry_encoding", "main_with_lib", "Claim in Viry.")) )
    (fun () -> ())
  in
  (* STEP 2: From CTRS rules to TRS rules and symbols. *)
  let flat_sym_add_l, (sym_add_l, r_l) = viry_encoding ctrs_r_l sign in
  if List.length ctrs_r_l > List.length r_l
  then raise (KaMeLoError (InternalError, "With_Viry_encoding", "main_with_lib", "Some rewriting rules disappear."))
  else (sort_l, sym_l, flat_sym_add_l, sym_add_l, r_l), sign
