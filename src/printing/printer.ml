
open Common.Type
open Common.Getter
open Mecanism.Count_data
open! LP.Syntax
open Interface.Output

open Translating

(* type output  = Format.formatter
type printer = output -> p_command -> unit *)

(** Lambdapi printer *)

let pp_import ppc cd prt : string list -> import -> unit = fun path i ->
  incr_real_import cd ;
  prt ppc (Main_translation.import_to_require_open path i)

let pp_sort ppc cd prt : sort -> unit = fun s ->
  (* incr_real_sort cd ; *)
  incr_real_symbol cd ;
  prt ppc (Main_translation.sort_to_p_symbol (pp s))

let pp_induc ppc cd prt : sort * symbol list -> unit = fun i ->
  incr_real_induc cd ;
  prt ppc (Main_translation.create_inductive_type i)

let pp_symbol ppc cd prt : symbol * attribute list -> unit =
  fun ((name, qv_l, p_l, p), attr_l) ->
  let s = (pp name, qv_l, p_l, p) in
  incr_real_symbol cd ;
  prt ppc (Main_translation.symbol_to_p_symbol s attr_l)

let pp_alias ppc cd prt :
      alias * (quant_var list * axiom * attribute list) option -> unit =
  fun v ->
  match v with
  | _, None -> () (* @TODO *)
  | al, Some(_,ax,_) ->
     try
       prt ppc (Main_translation.unconditional_rule_to_p_rule al ax) ;
       incr_real_rule cd
     with Axiom.ConditionalRule _ -> ()

let pp_alias_bis ppc prt al : unit = prt ppc (Alias.alias_to_definition al)

let pp_axiom ppc cd prt : quant_var list * axiom * attribute list -> unit =
  fun (_, ax, attr_l) ->
  match attr_l with
  | [Unit _] | [Assoc _] | [Idem _] ->
     (* if is_only_assoc ax then @TODO *)
     incr_real_rule cd ;
     prt ppc (Main_translation.equality_axiom_to_p_rule ax)
  | _ -> () (* @TODO *)

let pp_equality_axiom ppc cd prt : quant_var list * axiom -> unit =
  fun (_, ax) ->
  incr_real_rule cd ;
  prt ppc (Main_translation.equality_axiom_to_p_rule ax)

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
