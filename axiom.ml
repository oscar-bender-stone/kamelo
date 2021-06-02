
open LP_p_term
open Type
open Syntax
open Printer

type t = axiom

let rec is_predicate_axiom : axiom -> bool = fun a ->
  match a with
  | Equals(_,a1,a2)  -> is_predicate_axiom a1 || is_predicate_axiom a2
  | Exists(_,_,a)    -> is_predicate_axiom a
  | And(_,a1,a2)     -> is_predicate_axiom a1 || is_predicate_axiom a2
  | Or(_,a1,a2)      -> is_predicate_axiom a1 || is_predicate_axiom a2
  | Not(_,a)         -> is_predicate_axiom a
  | Implies(_,a1,a2) -> is_predicate_axiom a1 || is_predicate_axiom a2
  | Bottom   _ -> false
  | Top      _ -> false
  | Rewrites _ -> false (* users' rule *)
  | In(_,_,a)        -> is_predicate_axiom a
  | Dom_val  _ -> false
  | Predicat p -> match p with
                  | Sym(n,_,a_l) -> (* @TODO a_l ? *)
                     begin
                      try
                        let res = String.sub n 0 5 in String.equal res "Lblis"
                      with _ -> false
                     end
                  | Var _ -> false

let is_rule_axiom : axiom -> bool = fun a ->
  match a with
  | Rewrites _ -> true
  | _ -> false



(* GENRALISATION
let curry : ('a list -> 'b) -> ('b * 'a -> 'b) -> 'a list -> 'b = fun f g l ->
  let rec aux : 'a list -> (('a list -> 'b) -> 'b) -> 'b = fun l acc ->
    match l with
    | []   -> f_acc f
    | t::q -> aux q (fun x -> g((acc x), t) )
  in
  aux l (fun -> )
  *)
(* C'est juste un fold_left
let curry : (axiom list -> p_term) -> axiom list -> p_term = fun f l ->
  let rec aux : axiom list -> ((axiom list -> p_term) -> p_term) -> p_term =
    fun l f_acc ->
    match l with
    | []   -> f_acc f
    | t::q -> aux q (fun x -> P_Appl((f_acc x), t) )
  in
  aux l (fun -> )
 *)

let rec ax_curry : axiom -> p_term = fun a ->
  match a with
  | Predicat p ->
    begin
     match p with
     | Sym(n, _, a_l) ->
        let f = fun (a:p_term) (b:axiom) : p_term ->
                        Pos.none (P_Appl(a, ax_curry b))
        in
        List.fold_left f (create_ident n) a_l
     | Var(n, p) -> create_pattern_var n
    end
  | _ -> failwith "Not yet implemented."



(* Unit, Idem, comm, assoc *)
let of_equality_axiom : axiom -> p_rule = fun a ->
  match a with
  | Equals(_, a1, a2) ->
     Pos.none (ax_curry a1, ax_curry a2)
  | _ -> failwith "The current axiom isn't an equality one.\n
                   Please, raise an issue."
(*
let of_command : command -> = fun (c, a_l) ->
  match c with
  | Sort s | H_sort s ->
  | Symbol s | H_symbol s ->
  | Alias ->
  | Axiom(qv_l, a) ->
;;
 *)


(* Il n'y a rien qui indique que l'axiome a été généré car un symbole
   est un prédicat : il faut peut-être le rajouter ?
  let of_axiom : quant_var list * axiom * attribut list -> = fun qv_l a a_l ->*)

let of_axiom : quant_var list * axiom * attribut list -> attribut ->
               (quant_var list * axiom * attribut list) list ->
               (quant_var list * axiom * attribut list) list =
  fun (qv_l,a,a_l) attri ax_l ->
  match attri with
  | Subsort     _ -> ax_l   (* Cet axiome n'est pas pris en compte. *)
  | Functional  _ -> ax_l   (* Cet axiome n'est pas pris en compte. *)
  | Constructor _ -> ax_l   (* Cet axiome n'est pas pris en compte. *)
  | Assoc _ -> (qv_l,a,a_l)::ax_l (* @TODO Pour comparer avec LP : à enlever *)
  | Comm  _ -> (qv_l,a,a_l)::ax_l (* @TODO Pour comparer avec LP : à enlever *)
  | Idem  _ -> (qv_l,a,a_l)::ax_l
  | Unit  _ -> (qv_l,a,a_l)::ax_l
  | Initializer _ -> ax_l (* Cet axiome n'est pas pris en compte. *)
  | Owise       _ -> if is_predicate_axiom a then ax_l else (qv_l,a,a_l)::ax_l
  | Projection  _ -> ax_l (* Cet axiome n'est pas pris en compte. *)
  | _ -> (qv_l,a,a_l)::ax_l
