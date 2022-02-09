open Translating.Axiom
open Interface.LP_p_term

open Common.Type
open Common.Getter

open LP.Syntax
open Printer

open Mecanism.Iterator_plus_plus
open Mecanism.Count_data

open Translating.Eval_strategy

let rec printing_iter : (('a -> unit) * 'a list) list -> unit = function
  | []   -> ()
  | (prt, h)::q -> List.iter prt h ; printing_iter q

(* p_command list * p_symbol list * p_rule list *)

let encoding_with_Viry ppc cd prt : kommand list -> unit = fun kommand_l ->
  (* STEP 1: From K commands to CTRS rules (and partial printing). *)
  let f_sort _ acc s = pp_sort ppc cd prt s ; acc in
  let f_symbol attr_l acc s =
    (match is_constructor s attr_l with
     | Some sort ->
        let f new_v old_v = match old_v with None -> Some [new_v] | Some q -> Some (new_v::q) in
        data_induc := Induc.update sort (f s) !data_induc
     | None -> () ) ;
    pp_symbol ppc cd prt (s, attr_l) ; acc in
  let f_deleted  _ _ _ = [] in
  let propagation = fun _ x _ -> x in
  let f_subsort :
      attribute list -> ctrs_rule list -> quant_var list * axiom ->
      ctrs_rule list =
    fun _ _ (_, ax) -> collect_subsort_data ax ; []
  in
  let trans_implies =
    fun _ acc (_, ax) -> (of_implies_axiom ax)::acc
  in
  let ctrs_r_l =
    kommand_iter_without_alias cd kommand_l []
    f_sort f_deleted f_symbol f_deleted propagation
    ((fun attr_l acc al ax  -> trans_heating_rule  attr_l acc al ax),
     (fun attr_l acc al ax  -> trans_cooling_rule  attr_l acc al ax),
     (fun attr_l acc al ax  -> trans_semantic_rule attr_l acc al ax))
    propagation (f_subsort, propagation)
    (propagation, propagation, propagation, propagation)
    propagation propagation
    (propagation, trans_implies, trans_implies, trans_implies,
     propagation, trans_implies, trans_implies)
    (fun () -> ())
  in
  (* STEP 2: From CTRS rules to TRS rules and symbols. *)
  let sym_l, r_l = Translating.Viry.viry_encoding ctrs_r_l in
  (* STEP 3: Print symbols then TRS rules. *)
  if List.length sym_l > 3 then
    (List.iter
       (fun x -> incr_additional_symbol cd ; prt ppc (no_pos (P_symbol x)))
       (List.rev sym_l) ;
     List.iter
       (fun x -> incr_real_rule cd ; prt ppc (no_pos (P_rules  [x])))
       (List.rev r_l))
