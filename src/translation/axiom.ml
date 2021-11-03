
open LP_interface.LP_p_term
open Common.Type
open LP_interface.Syntax

open Common.Color

type t = axiom

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
(*
let rec map_append : 'a list -> ('a -> 'b) -> 'b list -> 'b list =
  fun l1 f l2 -> match l1 with
                 | [] -> l2
                 | h::t -> (f h)::(map_append t f l2)
 *)
let data_matching : p_term StrMap.t ref = ref StrMap.empty

let curry : (string -> p_term) -> t -> p_term = fun f_var ax ->
  let rec aux : t -> p_term = fun ax ->
    let f_sym = fun (a:p_term) (b:t) : p_term -> create_appl a (aux b) in
    match ax with
    | Predicat p ->
       begin
        match p with
        | Sym("inj", qv_l, a_l) ->
           let g p = match p with S x | Q x -> create_implicit_arg x in
           let tmp = List.map g qv_l in
           let res = List.fold_left create_appl (create_ident _INJ) tmp in
           List.fold_left f_sym res a_l
        | Sym(n, _, a_l) -> List.fold_left f_sym (create_ident n) a_l
        | Var(n, _) -> (if StrMap.mem n !data_matching
                        then StrMap.find n !data_matching
                        else f_var n)
       end
    | Dom_val(_, name) -> create_ident name
    (*| In _ -> failwith "OK, guys"
      | Equals _ -> failwith "EQUALS"
      | Exists _ -> failwith "EXISTS"
      | Or _ -> failwith "OR"
      | Not _ -> failwith "NOT"
      | Implies _ -> failwith "IMPLIES"
      | Bottom _ -> failwith "BOTTOM"
      | Top    _ -> failwith "TOP"
      | Rewrites _ -> failwith "REWRITES" *)
    | And (_, ax1, Predicat(Var(n,_))) ->
       let res = aux ax1 in
       data_matching := StrMap.add n res !data_matching ; res
    | _ -> failwith "Not yet implemented [Axiom.curry]."
  in
  aux ax

let curry_ident = curry create_ident
let curry_pattern = curry create_pattern_var

(* To translate Unit, Idem, comm, assoc *)
let of_equality_axiom : t -> p_rule = fun ax ->
  match ax with
  | Equals(_, ax1, ax2) ->
     (try
        no_pos (curry_pattern ax1, curry_pattern ax2)
      with _ -> failwith "Unit, Idem, comm, assoc")
  | _ -> failwith "The current axiom isn't an equality one.\n
                   Please, raise an issue."

  (* axiom{R} \implies{R} (
   *   \and{R}(
   *     \top{R}(),
   *     \and{R} (
   *         \in{SortBool{}, R} (
   *           X0:SortBool{},
   *           \dv{SortBool{}}("false")
   *         ),\and{R} (
   *         \in{SortBool{}, R} (
   *           X1:SortBool{},
   *           VarB:SortBool{}
   *         ),
   *         \top{R} ()
   *       ))),
   *   \and{R} (
   *     \equals{SortBool{},R} (
   *       Lbl'Unds'orBool'Unds'{}(X0:SortBool{},X1:SortBool{}),
   *       VarB:SortBool{}),
   *     \top{R}())) *)

(** Type of extra data about a rule *) (* Mettre aussi priority ? *)
type extra_data_rule =
   Uncond         (* A uncondtional rule *)
 | Cond of p_term (* A conditional rule with a condition *)
 | OwiseRule      (* A rule with the attribut "owise" *)

(** Type of a rule in a CTRS, which has the form
    ((LHS, RHS), extra_data_rule, priority) *)
type ctrs_rule = p_rule * extra_data_rule * int

(** [of_implies_axiom ax] translates the axiom [ax] which begins by "\implies"
    to a rewriting rule. *)
let of_implies_axiom : t -> ctrs_rule = fun ax ->
  let local_curry : (string -> p_term) -> t -> p_term StrMap.t -> p_term = fun f_var ax local_data ->
    let rec aux : t -> p_term = fun ax ->
      let f_sym = fun (a:p_term) (b:t) : p_term -> create_appl a (aux b) in
      match ax with
      | Predicat p ->
         begin
          match p with
          | Sym("inj", qv_l, a_l) ->
             let g p = match p with S x | Q x -> create_implicit_arg x in
             let tmp = List.map g qv_l in
             let res = List.fold_left create_appl (create_ident _INJ) tmp in
             List.fold_left f_sym res a_l
          | Sym(n, _, a_l) -> List.fold_left f_sym (create_ident n) a_l
          | Var(n, _) -> (if StrMap.mem n local_data
                          then StrMap.find n local_data
                          else f_var n)
         end
      | Dom_val(_, name) -> create_ident name
      (*| In _ -> failwith "OK, guys"
      | Equals _ -> failwith "EQUALS"
      | Exists _ -> failwith "EXISTS"
      | Or _ -> failwith "OR"
      | Not _ -> failwith "NOT"
      | Implies _ -> failwith "IMPLIES"
      | Bottom _ -> failwith "BOTTOM"
      | Top    _ -> failwith "TOP"
      | Rewrites _ -> failwith "REWRITES"
    | And (_, ax1, Predicat(Var(n,_))) ->
       let res = aux ax1 in
       data_matching := StrMap.add n res !data_matching ; res *)
      | _ -> failwith "Not yet implemented [local_curry]."
    in
    aux ax
  in
  let local_curry = local_curry create_pattern_var in
  let rec collect : t -> p_term StrMap.t -> p_term StrMap.t = fun ax acc ->
    match ax with
    | Top _ -> acc
    | In(_,(v,_), And(_, Dom_val(x,"false"), _)) | In(_,(v,_), And(_, _, Dom_val(x,"false"))) ->
       StrMap.add v (local_curry (Dom_val(x,"false")) acc) acc
    | In(_,(v,_),a) -> StrMap.add v (local_curry a acc) acc
    | And(_,Top _,ax) | And(_,ax,Top _) -> collect ax acc
    | And(_, ax1, ax2) -> collect ax2 (collect ax1 acc)

       (* (match a with
                    | Predicat (Var(n, _)) ->
                       StrMap.add v (create_ident n) local_data
                    | Predicat (Sym) ->
                       kseq{}(inj{SortMap{}, SortKItem{}}(VarK:SortMap{}),dotk{}())
                    | Dom_val(_, n) ->
                       StrMap.add v (create_ident n) local_data
                    | _ -> failwith "Fatal Error in [collect].") *)
    | _ -> failwith "Not yet implemented [collect]."
  in
  let data = StrMap.empty in
  match ax with
  | Implies(_, And(_,Top _, a1), And(_, Equals(_,l,r), Top _)) ->
     (let data = collect a1 data in
      try no_pos (local_curry l data, local_curry r data), Uncond, 42
      with _ -> failwith "Implies axiom")
  | Implies(_, And(_, Equals(_, c, Dom_val(_,"true")), a1), And(_, Equals(_,l,r), Top _)) ->
     (let data = collect a1 data in
      try no_pos (local_curry l data, local_curry r data), Cond (curry_pattern c), 42
      with _ -> failwith "Implies axiom")
  | Implies(_, Equals(_, c, Dom_val(_,"true")), And(_, Equals(_,l,r), Top _)) ->
      (try no_pos (local_curry l data, local_curry r data), Cond (curry_pattern c), 42
       with _ -> failwith "Implies axiom")
  | _ -> failwith "The current axiom isn't an implies one.\n
                   Please, raise an issue."


let rec is_predicate : t -> bool = fun a ->
  match a with
  | Equals(_,a1,a2)  -> is_predicate a1 || is_predicate a2
  | Exists(_,_,a)    -> is_predicate a
  | And(_,a1,a2)     -> is_predicate a1 || is_predicate a2
  | Or(_,a1,a2)      -> is_predicate a1 || is_predicate a2
  | Not(_,a)         -> is_predicate a
  | Implies(_,a1,a2) -> is_predicate a1 || is_predicate a2
  | Bottom   _ -> false
  | Top      _ -> false
  | Rewrites _ -> false (* users' rule *)
  | In(_,_,a)        -> is_predicate a
  | Dom_val  _ -> false
  | Predicat p -> match p with
                  | Sym(n, _, _) -> (* @TODO (n,_,a_l) ? *)
                     begin
                      try
                        let res = String.sub n 0 5 in String.equal res "Lblis"
                      with _ -> false
                     end
                  | Var _ -> false

let is_rule : t -> bool = fun a ->
  match a with
  | Rewrites _ -> true
  | _ -> false

let is_conditional_rule : t -> bool = fun a ->
  match a with
  | Top _ -> false
  | _     -> true

exception KComputation of string
exception ConditionalRule of string

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
              Format.printf (yel "WARNING: K computation found\n") ; p_TYPE)
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
  let rule =
    try
      (* Be careful: the order of the computation is important
         because of references *)
      let lhs = create_LHS al in
      let rhs = create_RHS ax in
      (lhs, rhs)
    with ConditionalRule _ ->
      Format.printf (yel "WARNING: Conditional rewriting rule.\n") ;
      (p_TYPE, p_TYPE)
  in
  no_pos rule
