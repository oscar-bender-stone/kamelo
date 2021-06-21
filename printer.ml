
open Syntax
open Type
open LP_printer
open Display_console
open Output

let pp_import : Format.formatter -> count_data -> string list -> import -> unit =
  fun ppf cd path i ->
  incr_real_import cd ;
  pp_command ppf (Translate.import_to_require_open path i)

let pp_sort : Format.formatter -> count_data -> sort -> unit =
  fun ppf cd s -> incr_real_sort cd ;
                  pp_command ppf (Translate.sort_to_p_symbol (pp s))

let pp_induc : Format.formatter -> count_data -> sort * symbol list -> unit =
  fun ppf cd i -> incr_real_induc cd ; pp_command ppf (Translate.create_inductive_type i)

let pp_symbol : Format.formatter -> count_data -> symbol * attribute list -> unit =
  fun ppf cd ((name, qv_l, p_l, p), attr_l) ->
  let s = (pp name, qv_l, p_l, p) in
  incr_real_symbol cd ;
  pp_command ppf (Translate.symbol_to_p_symbol s attr_l)


let pp_alias : Format.formatter -> count_data ->
                  alias * (quant_var list * axiom * attribute list) option -> unit =
  fun ppf cd v ->
  match v with
  | _, None -> () (* @TODO *)
  | al, Some(_,ax,_) ->
     try
       pp_command ppf (Translate.unconditional_rule_to_p_rule al ax) ;
       incr_real_rule cd
     with Axiom.ConditionalRule _ -> ()

let pp_axiom : Format.formatter -> count_data -> quant_var list * axiom * attribute list -> unit =
  fun ppf cd (qv_l, ax, attr_l) ->
  match attr_l with
  | Unit _::nil | Assoc _::nil | Idem _::nil ->
     (* if is_only_assoc ax then @TODO *)
     incr_real_rule cd ;
     pp_command ppf (Translate.equality_axiom_to_p_rule ax)
  | _ -> () (* @TODO *)


let pp_command : Format.formatter -> count_data -> command -> unit = fun ppf cd (c, attr_l) ->
  match c with
  | Sort   s | H_sort   s -> pp_sort ppf cd s
  | Symbol s | H_symbol s -> pp_symbol ppf cd (s, attr_l)
  | Alias al -> () (* @TODO : aller voir la suite de la liste *)
  | Axiom(qv_l, ax) -> pp_axiom ppf cd (qv_l, ax, attr_l)
