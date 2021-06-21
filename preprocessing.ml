open Type
open Color

open Symbol
open Axiom

open Display_console
open Printer

let old = ref false

module Sort = struct
  type t = sort
  let compare = String.compare
end
module Induc = Map.Make(Sort)

;; (* ( sort |-> symbol list) *)

let check_induc = ref false

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
  | t::q as l ->
     Format.printf (yel "WARNING: The symbol %s has new attribut(s): ") name;
     List.iter (fun n -> Format.printf (yel "%s ") n) l;
     Format.printf (yel ".\n")

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
       | H_sort s -> incr_k_hooked_sort cd ; aux q acc
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
       | H_symbol s -> incr_k_hooked_symbol cd ; aux q acc
       | Alias al->
          (match q with
           | [] -> incr_k_alias cd ; aux q (sort_l, induc_m, sym_l, (al, None)::alias_l, ax_l)
           | h::tl ->
              (match h with
               | Axiom(qv,a), attr_l ->
                  if is_rule_axiom a
                  then
                    (incr_k_rule cd ;
                     aux tl (sort_l, induc_m, sym_l, (al, Some(qv,a,attr_l))::alias_l, ax_l))
                  else
                    (incr_k_alias cd ;
                     aux q  (sort_l, induc_m, sym_l, (al, None)::alias_l, ax_l))
               | _ -> incr_k_alias cd ; aux q  (sort_l, induc_m, sym_l, (al, None)::alias_l, ax_l)))
       | Axiom(qv,a) ->
          incr_k_axiom cd ;
          match attr_l with
          | [] -> if is_predicate_axiom a
                  then aux q acc
                  else aux q (sort_l, induc_m, sym_l, alias_l, (qv,a,attr_l)::ax_l)
          | [t] ->
             let res = of_axiom (qv,a,attr_l) t ax_l in
             aux q (sort_l, induc_m, sym_l, alias_l, res)
          | _ -> aux q (sort_l, induc_m, sym_l, alias_l, (qv,a,attr_l)::ax_l)  (* failwith "Not yet implemented" *)
  in
  let sort_l, induc_m, sym_l, alias_l, ax_l = aux c_l ([], Induc.empty, [], [], []) in
  (name, List.rev sort_l, induc_m, List.rev sym_l, List.rev alias_l, List.rev ax_l)

let old : Format.formatter -> kmodule -> count_data -> unit = fun ff m cd ->

  let _, sort_l, induc_m, sym_l, alias_l, ax_l = preprocessing m cd in

  (* let import_l = if Induc.is_empty induc_m then import_l else ("prelude", [])::import_l in *)

  List.iter (pp_sort ff cd) sort_l;
  List.iter (pp_induc ff cd) (List.rev (Induc.bindings induc_m));
  List.iter (pp_symbol ff cd) sym_l;
  (*List.iter (trans_command ff) command_l;*)
  List.iter (pp_alias ff cd) alias_l;
  List.iter (pp_axiom ff cd) ax_l;
  (*List.iter (trans_command Format.std_formatter) command_l;*)
