open Common.Type
open Common.Getter
open Common.Error

open Translating.Axiom

open Mecanism.Count_data
open Interface.Output
open Interface.LP_p_term
open LP.Syntax
open LP.LP_printer

exception KComputation of string
exception ConditionalRule of string

let check_induc = ref false

module Sort = struct
  type t = sort
  let compare = String.compare
end
module Induc = Map.Make(Sort) (* ( sort |-> symbol list) *)

(** Inductive type *)
let induc_to_p_inductive : sort * symbol list -> p_inductive =
  fun (sort, s_l) ->
  (* p_inductive_aux = p_ident * p_term * (p_ident * p_term) list *)
  let f s = (create_p_ident (get_name s), Translating.Symbol.sym_curry s) in
  create_inductive (create_p_ident sort) p_TYPE (List.map f s_l)

(** [create_inductive_type i] creates non-mutual inductive type
    without parameter and position. *)
let create_inductive_type : sort * symbol list -> p_command = fun i ->
  create_LP_inductive [] [] [induc_to_p_inductive i]


(** [create_LHS al] creates a LHS of a rewriting rule thanks to an alias. *)
let create_LHS : alias -> p_term = fun al ->
  let get_def : alias -> def = fun (_,(_,_,_,def)) -> def in
  let def = get_def al in
  match def with
  | A a ->
     begin
      match a with
      | And(_,a1,a2) ->
         if is_conditional_rule a1 then
            raise (ConditionalRule "Conditional rewriting rule not supported yet.")
         else
           (try curry_pattern a2
            with KComputation _ ->
              wrn_msg _STDOUT "WARNING: K computation found." ; p_TYPE)
      (* _ -> failwith "LHS"*)
      |  _ -> failwith "In LHS: Not yet implemented"
     end
  | D _ -> failwith "Not possible in rewriting axiom"

(** [create_RHS ax] creates a RHS of a rewriting rule thanks to an axiom. *)
let create_RHS : t -> p_term = fun ax ->
  match ax with
  | Rewrites(_,_,And(_,a1,a2)) ->
     if is_conditional_rule a1 then
       raise (ConditionalRule "Conditional rewriting rule not supported yet.")
     else
       curry_pattern a2
  |  _ -> failwith "In RHS: Not yet implemented"

(** [create_rewriting_rule al ax] creates a rewriting rule thanks to
    an alias (for LHS) and an axiom (for RHS). *)
let create_rewriting_rule : alias -> t -> p_rule = fun al ax ->
  data_matching := StrMap.empty ;
  try
    (* Be careful: the order of the computation is important
       because of references *)
    let lhs = create_LHS al in
    let rhs = create_RHS ax in
    create_rule lhs rhs
  with ConditionalRule _ ->
    wrn_msg _STDOUT "WARNING: Conditional rewriting rule." ;
    create_rule p_TYPE p_TYPE

(** Axiom *)
let equality_axiom_to_p_rule : axiom -> p_command = fun ax ->
  create_multi_rule [of_equality_axiom ax]

(** Alias *)
let unconditional_rule_to_p_rule : alias -> axiom -> p_command =
  fun al ax -> create_multi_rule [create_rewriting_rule al ax]

(** Main (old) algorithm *)

(* 1 : remonter les symboles des types inductifs + descendre les sorts des types inductifs
 * 2 : Enlever les axiomes qui ne nous intéressent pas
 * [3] :  Regrouper ce qui va ensemble ? Risque de casser les dépendences ?
 * [4] : Regrouper au moins ce qui relève des configurations ? du sous-typage ?
 **)
let rec remove : 'a -> 'a list -> 'a list = fun a l ->
  match l with
  | [] -> []
  | t::q -> if t = a then q else t::(remove a q)

let print_new_attribute : name -> attribute list -> unit = fun name attri_l ->
  let rec aux = fun l acc ->
    match l with
    | [] -> acc
    | t::q -> (match t with
               | Other(n,_) -> aux q (n::acc)
               | _ -> aux q acc)
  in
  let res = aux attri_l [] in
  match res with
  | [] -> ()
  | _::_ as l ->
     wrn_1 _STDOUT "WARNING: The symbol %s has new attribut(s): " name;
     List.iter (fun n -> wrn_1 _STDOUT "%s " n) l;
     wrn_msg _STDOUT "."

(* Il n'y a rien qui indique que l'axiome a été généré car un symbole
   est un prédicat : il faut peut-être le rajouter ?
  let of_axiom : quant_var list * t * attribute list -> = fun qv_l ax a_l ->*)

let of_axiom : quant_var list * t * attribute list -> attribute ->
               (quant_var list * t * attribute list) list ->
               (quant_var list * t * attribute list) list =
  fun (qv_l, ax, a_l) attri ax_l ->
  match attri with
  | Subsort     _ -> ax_l   (* Cet axiome n'est pas pris en compte. *)
  | Projection  _ -> ax_l (* Cet axiome n'est pas pris en compte. *)
  | Functional  _ -> ax_l   (* Cet axiome n'est pas pris en compte. *)
  | Constructor _ -> ax_l   (* Cet axiome n'est pas pris en compte. *)
  | Assoc _ -> (qv_l,ax,a_l)::ax_l (* @TODO Pour comparer avec LP : à enlever *)
  | Comm  _ -> (qv_l,ax,a_l)::ax_l (* @TODO Pour comparer avec LP : à enlever *)
  | Idem  _ -> (qv_l,ax,a_l)::ax_l
  | Unit  _ -> (qv_l,ax,a_l)::ax_l
  | Initializer _ -> ax_l (* Cet axiome n'est pas pris en compte. *)
  | Owise       _ -> if is_predicate ax then ax_l else (qv_l,ax,a_l)::ax_l
  | _ -> (qv_l,ax,a_l)::ax_l


let preprocessing :
      kmodule -> count_data ->
      name * sort list * (symbol list) Induc.t * (symbol * attribute list) list *
        (alias * (quant_var list * axiom * attribute list) option) list *
          (quant_var list * axiom * attribute list) list =
  fun (name, _, c_l, _) cd ->
  let rec aux l ((sort_l, induc_m, sym_l, alias_l, ax_l) as acc) =
    match l with
    | [] -> acc
    | (c, attr_l)::q ->
       match c with
       | Sort   s ->
          incr_k_sort cd ; print_new_attribute s attr_l ;
          aux q (s::sort_l, induc_m, sym_l, alias_l, ax_l)
       | H_sort _ -> incr_k_hooked_sort cd ; aux q acc
       | Symbol s ->
          begin
            let name,_,_,_ = s in
            print_new_attribute name attr_l ;
            if not(!check_induc) then (incr_k_symbol cd ; aux q (sort_l, induc_m, (s,attr_l)::sym_l, alias_l, ax_l))
            else
              (match is_constructor s attr_l with
               | Some sort ->
                  let f new_v old_v = match old_v with None -> Some [new_v] | Some q -> Some (new_v::q) in
                  let induc_m = Induc.update sort (f s) induc_m in
                  aux q (remove sort sort_l, induc_m, sym_l, alias_l, ax_l)
               | None ->
                  aux q (sort_l, induc_m, (s,attr_l)::sym_l, alias_l, ax_l))
          end
       | H_symbol _ -> incr_k_hooked_symbol cd ; aux q acc
       | Alias al->
          (match q with
           | [] -> incr_k_alias cd ; aux q (sort_l, induc_m, sym_l, (al, None)::alias_l, ax_l)
           | h::tl ->
              (match h with
               | Axiom(qv,a), attr_l ->
                  if is_rule a
                  then
                    (incr_k_rewriting_ax cd ;
                     aux tl (sort_l, induc_m, sym_l, (al, Some(qv,a,attr_l))::alias_l, ax_l))
                  else
                    (incr_k_alias cd ;
                     aux q  (sort_l, induc_m, sym_l, (al, None)::alias_l, ax_l))
               | _ -> incr_k_alias cd ; aux q  (sort_l, induc_m, sym_l, (al, None)::alias_l, ax_l)))
       | Axiom(qv,a) ->
          incr_k_axiom cd ;
          match attr_l with
          | [] -> if is_predicate a
                  then aux q acc
                  else aux q (sort_l, induc_m, sym_l, alias_l, (qv,a,attr_l)::ax_l)
          | [t] ->
             let res = of_axiom (qv,a,attr_l) t ax_l in
             aux q (sort_l, induc_m, sym_l, alias_l, res)
          | _ -> aux q (sort_l, induc_m, sym_l, alias_l, (qv,a,attr_l)::ax_l)  (* failwith "Not yet implemented" *)
  in
  let sort_l, induc_m, sym_l, alias_l, ax_l = aux c_l ([], Induc.empty, [], [], []) in
  (name, List.rev sort_l, induc_m, List.rev sym_l, List.rev alias_l, List.rev ax_l)


(** Some printers *)

let pp_sort ppc cd prt : sort -> unit = fun s ->
  (* incr_real_sort cd ; *)
  incr_real_symbol cd ;
  prt ppc (Translating.Sort.sort_to_p_symbol (pp s))

let pp_induc ppc cd prt : sort * symbol list -> unit = fun i ->
  incr_real_induc cd ;
  prt ppc (create_inductive_type i)

let pp_symbol ppc cd prt : symbol * attribute list -> unit =
  fun ((name, qv_l, p_l, p), attr_l) ->
  let s = (pp name, qv_l, p_l, p) in
  incr_real_symbol cd ;
  prt ppc (Translating.Translation.symbol_to_p_symbol s attr_l)

let pp_alias ppc cd prt :
      alias * (quant_var list * axiom * attribute list) option -> unit =
  fun v ->
  match v with
  | _, None -> () (* @TODO *)
  | al, Some(_,ax,_) ->
     try
       prt ppc (unconditional_rule_to_p_rule al ax) ;
       incr_real_rule cd
     with ConditionalRule _ -> ()

let pp_axiom ppc cd prt : quant_var list * axiom * attribute list -> unit =
  fun (_, ax, attr_l) ->
  match attr_l with
  | [Unit _] | [Assoc _] | [Idem _] ->
     (* if is_only_assoc ax then @TODO *)
     incr_real_rule cd ;
     prt ppc (equality_axiom_to_p_rule ax)
  | _ -> () (* @TODO *)


(** To print the resulting translation *)

let first_translation ppc cd : kmodule -> unit = fun m ->
  let _, sort_l, induc_m, sym_l, alias_l, ax_l = preprocessing m cd in

  (* let import_l = if Induc.is_empty induc_m then import_l else ("prelude", [])::import_l in *)

  List.iter (pp_sort ppc cd pp_command) sort_l;
  List.iter (pp_induc ppc cd pp_command) (List.rev (Induc.bindings induc_m));
  List.iter (pp_symbol ppc cd pp_command) sym_l;
  (*List.iter (trans_command ppc) command_l;*)
  List.iter (pp_alias ppc cd pp_command) alias_l;
  List.iter (pp_axiom ppc cd pp_command) ax_l;
  (*List.iter (trans_command Format.std_formatter) command_l;*)
