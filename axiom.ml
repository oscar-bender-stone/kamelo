
open LP_p_term
open Type
open Syntax
open LP_printer

open Display_console

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

let is_rule_command : command -> bool = fun (c,_) ->
  match c with
  | Axiom(_,ax) ->
     (match ax with
      | Rewrites _ -> true
      | _ -> false)
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

let rec map_append : 'a list -> ('a -> 'b) -> 'b list -> 'b list =
  fun l1 f l2 -> match l1 with
                 | [] -> l2
                 | h::t -> (f h)::(map_append t f l2)

exception KComputation of string

let rec ax_curry : axiom -> p_term = fun a ->
  let f = fun (a:p_term) (b:axiom) : p_term -> create_appl a (ax_curry b) in
  match a with
  | Predicat p ->
    begin
     match p with
     | Sym("inj", qv_l, a_l) ->
        let g p = match p with S x | Q x -> create_implicit_arg x in
        let tmp = List.map g qv_l in
        let res = List.fold_left create_appl (create_ident "injG") tmp in
        List.fold_left f res a_l
     | Sym(n, _, a_l) -> List.fold_left f (create_ident n) a_l
     | Var(n, p) -> create_pattern_var n
    end
  | Dom_val(sort, name) -> create_ident name
  (*| In _ -> failwith "OK, guys"
  | Equals _ -> failwith "EQUALS"
  | Exists _ -> failwith "EXISTS"
  | Or _ -> failwith "OR"
  | Not _ -> failwith "NOT"
  | Implies _ -> failwith "IMPLIES"
  | Bottom _ -> failwith "BOTTOM"
  | Top    _ -> failwith "TOP"
  | Rewrites _ -> failwith "REWRITES" *)
  | And _ -> raise (KComputation "K computations not yet implemented.")
  | _ -> failwith "Not yet implemented, if the axiom isn't a predicate."



(* Unit, Idem, comm, assoc *)
let of_equality_axiom : axiom -> p_rule = fun a ->
  match a with
  | Equals(_, a1, a2) ->
     (try
        no_pos (ax_curry a1, ax_curry a2)
      with _ -> failwith "Unit, Idem, comm, assoc")
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
  let of_axiom : quant_var list * axiom * attribute list -> = fun qv_l a a_l ->*)

let of_axiom : quant_var list * axiom * attribute list -> attribute ->
               (quant_var list * axiom * attribute list) list ->
               (quant_var list * axiom * attribute list) list =
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
(*
type def = A of axiom | D of name * quant_var

type alias = symbol * (name * quant_var list * (name * param) list * def)
             *)
let is_conditional_rule : axiom -> bool = fun a ->
  match a with
  | Top _ -> false
  | _     -> true

exception ConditionalRule of string

let rec create_rewriting_rule : alias -> axiom -> p_rule = fun aw ax ->
  let get_def : alias -> def = fun (_,(_,_,_,def)) -> def in
  let def = get_def aw in
  (* Create the LHS thanks to the alias *)
  let lhs =
    match def with
    | A a ->
       begin
        match a with
        | And(_,a1,a2) ->
           if is_conditional_rule a1 then
             raise (ConditionalRule "Conditional rewriting rule not supported yet.")
           else
             (try ax_curry a2
              with KComputation _ ->
                Format.printf (yel "WARNING: K computation found\n") ; _TYPE)
                (* _ -> failwith "LHS"*)
        |  _ -> failwith "In LHS: Not yet implemented"
       end
    | D _ -> failwith "Not possible in rewriting axiom"
  in
  (* Create the RHS thanks to the axiom *)
  let rhs =
    match ax with
    | Rewrites(_,_,And(_,a1,a2)) ->
       if is_conditional_rule a1 then
         raise (ConditionalRule "Conditional rewriting rule not supported yet.")
       else
         ax_curry a2
    |  _ -> failwith "In RHS: Not yet implemented"
  in
  no_pos (lhs, rhs)
