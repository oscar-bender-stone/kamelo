
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
  fun ppf cd (_, ax, attr_l) ->
  match attr_l with
  | [Unit _] | [Assoc _] | [Idem _] ->
     (* if is_only_assoc ax then @TODO *)
     incr_real_rule cd ;
     pp_command ppf (Translate.equality_axiom_to_p_rule ax)
  | _ -> () (* @TODO *)

let pp_command : Format.formatter -> count_data -> command -> unit = fun ppf cd (c, attr_l) ->
  match c with
  | Sort     s -> incr_k_sort cd        ; pp_sort ppf cd s
  | H_sort   s -> incr_k_hooked_sort cd ; pp_sort ppf cd s
  | Symbol   s -> incr_k_symbol cd        ; pp_symbol ppf cd (s, attr_l)
  | H_symbol s -> incr_k_hooked_symbol cd ; pp_symbol ppf cd (s, attr_l)
  | Alias    _ -> incr_k_alias cd (* @TODO : aller voir la suite de la liste *)
  | Axiom(qv_l, ax) -> incr_k_axiom cd ; pp_axiom ppf cd (qv_l, ax, attr_l)

let pp_command_bis  : Format.formatter -> count_data -> command list -> unit = fun ppf cd command_l ->
  let f_axiom :
        Format.formatter -> count_data -> attribute list -> unit -> quant_var list * axiom -> unit =
    fun ppf cd attr_l _ (qv_l, ax) ->
    match attr_l with
    | [] -> if Axiom.is_predicate_axiom ax then ()
            else pp_axiom ppf cd (qv_l, ax, attr_l)
    | _ -> pp_axiom ppf cd (qv_l, ax, attr_l)
  in
  kore_command_iter cd command_l ()
    (fun _ _ s -> pp_sort ppf cd s) (fun _ _ s -> pp_sort ppf cd s)
    (fun attr_l _ s -> pp_symbol ppf cd (s, attr_l)) (fun attr_l _ s -> pp_symbol ppf cd (s, attr_l))
    (fun _ _ _ -> ()) (fun attr_l _ ({lhs=al;rhs=(qv_l, ax)}) -> pp_alias ppf cd (al, Some (qv_l, ax, attr_l))) (f_axiom ppf cd)
