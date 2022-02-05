
open Interface.LP_p_term
open Interface.K_prelude
open LP.Syntax

open Common.Type
open Common.Getter
open Common.Error

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
(** ****************************************************** *)
(** To translate exists-axioms (functional or subsort one) *)
(** ****************************************************** *)

(** Currently, functional axioms aren't used and
    subsort axioms are just used to change some injections. *)

module StrMap = Map.Make(String)

(** où key est une sous-sorte des sortes dans la liste *)
let subsort_data : (string list) StrMap.t ref = ref StrMap.empty

let from_subsort_axiom : string -> string -> unit = fun s1 s2 ->
  let f a = match a with
    | None   -> Some [s2]   (* Si l'entrée n'existait pas encore *)
    | Some l -> Some(s2::l) (* Si l'entrée existait déjà *)
  in (* TODO a factorisé avec [find_equiv_class] dans viry.ml *)
  subsort_data := StrMap.update s1 f !subsort_data

let free_var : (string list) StrMap.t ref = ref StrMap.empty (* TODO remove *)

let data_matching : p_term StrMap.t ref = ref StrMap.empty (* TODO remove *)

let do_specific_thing : bool ref = ref false

(* Par rajouter une injection à HOLE *)
let init_var : string * p_term = ("", p_TYPE)
let specific_var : (string * p_term) ref = ref init_var
let reset_var : unit -> unit = fun () -> specific_var := init_var

let change_sort_inj : p_term -> p_term = fun t ->
  let rec aux t = match t with
    | P_Appl(
        {elt=P_Appl(
            {elt=P_Appl({elt=P_Iden({elt=(x1,"inj");pos=x2}, x3);pos=x4},
                        {elt=P_Expl({elt=P_Iden ({elt=(x5,s1) ;pos=x6}, x7); pos=x8}) ; pos=x9} )
            ; pos=x10},
            {elt=P_Expl({elt=P_Iden ({elt=(x11,s2) ;pos=x12}, x13); pos=x14}) ; pos=x15} )
        ; pos=x16},
        {elt=P_Patt(Some {elt=("HOLE" as n) ;pos=x17}, x18) ; pos=x19} ) ->
       let f : string -> string list -> string -> string = fun key v acc ->
         if List.mem _SORT_KRESULT v && List.mem s1 v
         then key ^ acc
         else "" ^ acc
       in
       let new_s = StrMap.fold f !subsort_data "" in
       let new_s = if new_s = "" then s1 else new_s in
       let res s2 =
         P_Appl(
             {elt=P_Appl(
                      {elt=P_Appl({elt=P_Iden({elt=(x1,"inj");pos=x2}, x3);pos=x4},
                                  {elt=P_Expl({elt=P_Iden ({elt=(x5,new_s) ;pos=x6}, x7); pos=x8}) ; pos=x9} )
                      ; pos=x10},
                      {elt=P_Expl({elt=P_Iden ({elt=(x11,s2) ;pos=x12}, x13); pos=x14}) ; pos=x15} )
             ; pos=x16},
             {elt=P_Patt(Some {elt=n ;pos=x17}, x18) ; pos=x19} )
       in
       if not(new_s = s1) then specific_var := (n, LP.Pos.none (res s1)) ;
       res s2
    | P_Appl(({elt=t1;pos=x1}), ({elt=t2 ;pos=x2})) ->
       P_Appl(({elt=aux t1;pos=x1}), ({elt=aux t2 ;pos=x2}))
    | _ -> t
  in
  {elt=aux t.elt ; pos= t.pos}

let curry : (string -> p_term) -> t -> p_term = fun f_var ax ->
  let rec aux : t -> p_term = fun ax ->
    let f_sym = fun (a:p_term) (b:t) : p_term -> create_appl a (aux b) in
    match ax with
    | Predicate p ->
       begin
        match p with
        | Sym("inj", qv_l, a_l) ->
           let g p = match p with S x | Q x -> create_implicit_arg x in
           let tmp = List.map g qv_l in
           let res = List.fold_left create_appl p_INJ tmp in
           let res = List.fold_left f_sym res a_l in
           if !do_specific_thing
           then change_sort_inj res
           else res
        | Sym(n, _, a_l) -> List.fold_left f_sym (create_ident n) a_l
        | Var(n, _) -> (if StrMap.mem n !data_matching
                        then StrMap.find n !data_matching
                        else
                          (if !do_specific_thing
                           then
                             ( (* wrn_1 "\nSpecific var: %s") (fst !specific_var) ;
                                  wrn_1 "\nSpecific var: %s\n" n ; *)
                              if (fst !specific_var) = (Interface.Output.pp n)
                              then snd !specific_var
                              else change_sort_inj (f_var n))
                           else f_var n))
       end
    | Dom_val("SortId", name) ->
       let f a = match a with
         | None   -> Some [name]   (* Si l'entrée n'existait pas encore *)
         | Some l -> if List.mem name l then Some l else Some(name::l) (* Si l'entrée existait déjà *)
       in (* TODO a factorisé avec [find_equiv_class] dans viry.ml et [from_subsort_axiom] plus bas *)
       free_var := StrMap.update "SortId" f !free_var ; create_ident name
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
    | And (_, ax1, Predicate(Var(n,_))) ->
       let res = aux ax1 in
       data_matching := StrMap.add n res !data_matching ; res
    | _ -> failwith "Not yet implemented [Axiom.curry]."
  in
  aux ax

let curry_ident = curry create_ident
let curry_pattern = curry create_pattern_var

(** **************************************************** *)
(** To translate equals-axioms
    (Associative, Commutative, Unit and Idempotence one) *)
(** **************************************************** *)

let of_equality_axiom : t -> p_rule = fun ax ->
  data_matching := StrMap.empty ;
  match ax with
  | Equals(_, ax1, ax2) ->
     (try
        no_pos (curry_pattern ax1, curry_pattern ax2)
      with _ -> failwith "Unit, Idem, comm, assoc")
  | _ -> failwith "The current axiom isn't an equality one.\n
                   Please, raise an issue."

(** **************************************************** *)
(** To translate or-axioms, bottom-axioms, not-axioms
    (aka constructor one) *)
(** **************************************************** *)

(** Currently, these axioms aren't used. *)

(** ****************************** *)
(** To translate implies-axioms    *)
(** ****************************** *)

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
 | Uncond         (* A uncondtional rule *)
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
      | Predicate p ->
         begin
          match p with
          | Sym("inj", qv_l, a_l) ->
             let g p = match p with S x | Q x -> create_implicit_arg x in
             let tmp = List.map g qv_l in
             let res = List.fold_left create_appl p_INJ tmp in
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
      | Rewrites _ -> failwith "REWRITES" *)
    | And (_, ax1, Predicate(Var(n,_))) ->
       let res = aux ax1 in
       data_matching := StrMap.add n res !data_matching ; res
      | _ -> failwith "Not yet implemented [local_curry]."
    in
    aux ax
  in
  let local_curry = local_curry create_pattern_var in
  let rec collect : t -> p_term StrMap.t -> p_term StrMap.t = fun ax acc ->
    match ax with
    | Top _ -> acc
    | In(_,(v1,_), And(_, Dom_val(x,"false"), Predicate(Var(v2, _))))
         | In(_,(v1,_), And(_, Predicate(Var(v2, _)), Dom_val(x,"false"))) ->
       let tmp = StrMap.add v1 (local_curry (Dom_val(x,"false")) acc) acc in
       StrMap.add v2 (local_curry (Dom_val(x,"false")) tmp) tmp
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
      try no_pos (local_curry l data, local_curry r data), Cond (local_curry c data), 42
      with _ -> failwith "Implies axiom")
  | Implies(_, Equals(_, c, Dom_val(_,"true")), And(_, Equals(_,l,r), Top _)) ->
      (try no_pos (local_curry l data, local_curry r data), Cond (local_curry c data), 42
       with _ -> failwith "Implies axiom")
  | _ -> failwith "The current axiom isn't an implies one.\n
                   Please, raise an issue."

(** ********************************** *)
(** To translate rewriting axioms      *)
(** ********************************** *)

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
  let rule =
    try
      (* Be careful: the order of the computation is important
         because of references *)
      let lhs = create_LHS al in
      let rhs = create_RHS ax in
      (lhs, rhs)
    with ConditionalRule _ ->
      wrn_msg _STDOUT "WARNING: Conditional rewriting rule." ;
      (p_TYPE, p_TYPE)
  in
  no_pos rule

(** To store the type of each sort   *)
let sort_signature : p_term StrMap.t ref = ref StrMap.empty

(** [create_isKResult_rule] generates a rewriting rule
    p_IS_KRESULT (p_KSEQ (p_INJ { s } _) p_DOTK) --> b
    b = true,  if s is a subsort of KResult
    b = false, otherwise *)
let create_isKResult_rule : unit -> p_rule list = fun () ->
  let create_one_LHS : string -> p_term = fun sort_x ->
    let inj = create_appl (create_appl p_INJ (create_implicit_arg sort_x)) p_WILD in
    let k_comput = create_appl (create_appl p_KSEQ inj) p_DOTK in
    create_appl p_IS_KRESULT k_comput
  in
  let create_one_rule : string -> p_term -> p_rule list -> p_rule list =
    fun key _ acc ->
    if key = _SORTK || key = _SORT_KITEM || key = _SORT_KRESULT then acc
    else
      let lhs = create_one_LHS key in
      let subsort_rel = try StrMap.find key !subsort_data with Not_found -> [] in
      let rhs = create_ident (string_of_bool (List.mem _SORT_KRESULT subsort_rel)) in
      (no_pos (lhs, rhs))::acc
  in
  StrMap.fold create_one_rule !sort_signature []
