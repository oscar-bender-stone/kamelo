
open Common.Type
open Mecanism.Count_data
open! LP.Syntax
open Interface.Output

open Translation

(* type output  = Format.formatter
type printer = output -> p_command -> unit *)

(** Lambdapi printer *)

let pp_import ppc cd prt : string list -> import -> unit = fun path i ->
  incr_real_import cd ;
  prt ppc (Translate.import_to_require_open path i)

let pp_sort ppc cd prt : sort -> unit = fun s ->
  (* incr_real_sort cd ; *)
  incr_real_symbol cd ;
  prt ppc (Translate.sort_to_p_symbol (pp s))

let pp_induc ppc cd prt : sort * symbol list -> unit = fun i ->
  incr_real_induc cd ;
  prt ppc (Translate.create_inductive_type i)

let pp_symbol ppc cd prt : symbol * attribute list -> unit =
  fun ((name, qv_l, p_l, p), attr_l) ->
  let s = (pp name, qv_l, p_l, p) in
  incr_real_symbol cd ;
  prt ppc (Translate.symbol_to_p_symbol s attr_l)

let pp_alias ppc cd prt :
      alias * (quant_var list * axiom * attribute list) option -> unit =
  fun v ->
  match v with
  | _, None -> () (* @TODO *)
  | al, Some(_,ax,_) ->
     try
       prt ppc (Translate.unconditional_rule_to_p_rule al ax) ;
       incr_real_rule cd
     with Axiom.ConditionalRule _ -> ()

let pp_alias_bis ppc prt al : unit = prt ppc (Alias.alias_to_definition al)

let pp_axiom ppc cd prt : quant_var list * axiom * attribute list -> unit =
  fun (_, ax, attr_l) ->
  match attr_l with
  | [Unit _] | [Assoc _] | [Idem _] ->
     (* if is_only_assoc ax then @TODO *)
     incr_real_rule cd ;
     prt ppc (Translate.equality_axiom_to_p_rule ax)
  | _ -> () (* @TODO *)

let pp_equality_axiom ppc cd prt : quant_var list * axiom -> unit =
  fun (_, ax) ->
  incr_real_rule cd ;
  prt ppc (Translate.equality_axiom_to_p_rule ax)

let pp_axiom_bis ppc _ (* TODO fix *) prt : quant_var list * axiom -> unit = fun (_,ax) ->
  match ax with
    | Rewrites(_,lhs,And(_,a1,a2)) ->
       if is_conditional_rule a1 then
         raise (Axiom.ConditionalRule "Conditional rewriting rule not supported yet.")
       else
         prt ppc (Interface.LP_p_term.no_pos (P_rules [Interface.LP_p_term.no_pos (Axiom.curry_pattern lhs, Axiom.curry_pattern a2)]))
    |  _ -> failwith "In RHS: Not yet implemented"

let pp_kommand ppc cd prt : kommand -> unit = fun (kommand, attr_l) ->
  match kommand with
  | Sort          s -> pp_sort ppc cd prt s
  | H_sort        s -> pp_sort ppc cd prt s
  | Symbol        s -> pp_symbol ppc cd prt (s, attr_l)
  | H_symbol      s -> pp_symbol ppc cd prt (s, attr_l)
  | Alias        al -> pp_alias_bis ppc prt al (* @TODO : aller voir la suite de la liste *)
  | Axiom(qv_l, ax) -> pp_axiom ppc cd prt (qv_l, ax, attr_l)

(*

let pp_kommand_bis  : output -> count_data -> printer -> kommand list -> unit = fun ppf cd prt kommand_l ->
  let do_nothing = fun _ _ _ -> () in
  let equality_axiom = fun _ _ (qv_l, ax) -> pp_equality_axiom ppf cd prt (qv_l, ax) in
  let f_axiom : attribute list -> unit -> quant_var list * axiom -> unit =
    fun attr_l _ (qv_l, ax) ->
    match attr_l with
    | [] -> if Axiom.is_predicate ax then ()
            else pp_axiom ppf cd prt (qv_l, ax, attr_l)
    | _ -> pp_axiom ppf cd prt (qv_l, ax, attr_l)
  in
  kommand_iter_without_alias cd kommand_l ()
  (fun _ _ s -> pp_sort ppf cd prt s) (fun _ _ s -> pp_sort ppf cd prt s)
  (fun attr_l _ s -> pp_symbol ppf cd prt (s, attr_l)) (fun attr_l _ s -> pp_symbol ppf cd prt (s, attr_l))
  do_nothing (fun attr_l _ ({lhs=al;rhs=(qv_l, ax)}) -> pp_alias ppf cd prt (al, Some (qv_l, ax, attr_l)))
  f_axiom
  (do_nothing, do_nothing, do_nothing, do_nothing, do_nothing,
   equality_axiom, equality_axiom, equality_axiom, equality_axiom,
   do_nothing, do_nothing) (fun () -> ())

let pp_kommand_ter : output -> count_data -> printer -> kommand list -> unit  = fun ppf cd prt kommand_l ->
  let do_nothing : attribute list -> 'a -> quant_var list * axiom -> 'a = fun _ acc _ -> acc in
  let equality_axiom = fun _ _ (qv_l, ax) -> pp_equality_axiom ppf cd prt (qv_l, ax) in
   let f_axiom : attribute list -> unit -> quant_var list * axiom -> unit =
    fun attr_l _ (qv_l, ax) ->
    match attr_l with
    | [] -> if Axiom.is_predicate ax then ()
            else pp_axiom ppf cd prt (qv_l, ax, attr_l)
    | _ -> pp_axiom ppf cd prt (qv_l, ax, attr_l)
  in
  kommand_iter_with_alias cd kommand_l ()
  (fun _ _ s -> pp_sort ppf cd prt s) (fun _ _ s -> pp_sort ppf cd prt s)
  (fun attr_l _ s -> pp_symbol ppf cd prt (s, attr_l)) (fun attr_l _ s -> pp_symbol ppf cd prt (s, attr_l))
  (fun _ _ al -> pp_alias_bis ppf prt al) (fun _ _ ax -> pp_axiom_bis ppf cd prt ax) f_axiom
  (do_nothing, do_nothing, do_nothing, do_nothing, do_nothing,
   equality_axiom, equality_axiom, equality_axiom, equality_axiom,
   do_nothing, (fun attr_l -> f_axiom attr_l)) (fun () -> ())

  *)
