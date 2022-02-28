
open Interface.LP_p_term
open Interface.K_prelude
open LP.Syntax

open Common.Type
open Common.Error
open Common.Xlib_OCaml

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

(** où key est une sous-sorte des sortes dans la liste *)
let subsort_data : (string list) StrMap.t ref = ref StrMap.empty

let from_subsort_axiom : string -> string -> unit = fun s1 s2 ->
  subsort_data := add_update s1 s2 !subsort_data

(** [collect_subsort_data ax] updates the database of subsorts
    For instance, if this function matches an axiom with the following shape:
      \exists{R} (Val:SortKItem{},
                  \equals{SortKItem{}, R}
                      (Val:SortKItem{},
                       inj{SortCell{}, SortKItem{}} (From:SortCell{})))
    then it adds the subsort relation SortCell <: SortKItem.
    Otherwise, it raises an error. *)
  let collect_subsort_data : axiom -> unit = function
    | Exists (_, _, Equals(_, _, Predicate(Sym(s, [S s1; S s2], _)))) when s = _INJ ->
       from_subsort_axiom s1 s2
    | _ -> raise (InternalError "Need to update [Axiom.collect_subsort_data].")

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
            {elt=P_Appl({elt=P_Iden({elt=(x1, s);pos=x2}, x3);pos=x4},
                        {elt=P_Expl({elt=P_Iden ({elt=(x5,s1) ;pos=x6}, x7); pos=x8}) ; pos=x9} )
            ; pos=x10},
            {elt=P_Expl({elt=P_Iden ({elt=(x11,s2) ;pos=x12}, x13); pos=x14}) ; pos=x15} )
        ; pos=x16},
        {elt=P_Patt(Some {elt=("HOLE" as n) ;pos=x17}, x18) ; pos=x19} ) when s = _INJ ->
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
                      {elt=P_Appl({elt=P_Iden({elt=(x1, _INJ);pos=x2}, x3);pos=x4},
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
        | Sym(n, qv_l, a_l) ->
           if n = _INJ then
             let g p = match p with S x | Q x -> create_implicit_arg x in
             let tmp = List.map g qv_l in
             let res = List.fold_left create_appl p_INJ tmp in
             let res = List.fold_left f_sym res a_l in
             if !do_specific_thing
             then change_sort_inj res
             else res
           else
             List.fold_left f_sym (create_ident n) a_l
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
    | Dom_val(s, name) ->
       if s = _SORT_ID then
         free_var := add_update_without_dup _SORT_ID name !free_var ;
       create_ident name
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
    | _ -> raise (NotYetImplemented "Need to update [Axiom.curry].")
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
        create_rule (curry_pattern ax1) (curry_pattern ax2)
      with _ -> raise (InternalError "Need to update [Axiom.of_equality_axiom]."))
  | _ -> raise (InternalError "The current axiom isn't an equality one.\n
                Please, raise an issue.")

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
          | Sym(n, qv_l, a_l) ->
             if n = _INJ then
               let g p = match p with S x | Q x -> create_implicit_arg x in
               let tmp = List.map g qv_l in
               let res = List.fold_left create_appl p_INJ tmp in
               List.fold_left f_sym res a_l
             else
               List.fold_left f_sym (create_ident n) a_l
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
      | Not(_, In(_, (v,_), a)) -> raise (NotYetImplemented "TODO")
      | And (_, ax1, Predicate(Var(n,_))) ->
         let res = aux ax1 in
         data_matching := StrMap.add n res !data_matching ; res
      | _ -> raise (NotYetImplemented "Need to update [Axiom.local_curry].")
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
    | _ -> raise (NotYetImplemented "Need to update [Axiom.collect].")
  in
  let data = StrMap.empty in
  match ax with
  | Implies(_, And(_,Top _, a1), And(_, Equals(_,l,r), Top _)) ->
     (let data = collect a1 data in
      try create_rule (local_curry l data) (local_curry r data), Uncond, 42
      with _ -> raise (InternalError "Function [Axiom.of_implies_axiom] - Case 1"))
  | Implies(_, And(_, Equals(_, c, Dom_val(_,"true")), a1), And(_, Equals(_,l,r), Top _)) ->
     (let data = collect a1 data in
      try create_rule (local_curry l data) (local_curry r data), Cond (local_curry c data), 42
      with _ -> raise (InternalError "Function [Axiom.of_implies_axiom] - Case 2"))
  | Implies(_, Equals(_, c, Dom_val(_,"true")), And(_, Equals(_,l,r), Top _)) ->
      (try create_rule (local_curry l data) (local_curry r data), Cond (local_curry c data), 42
       with _ -> raise (InternalError "Function [Axiom.of_implies_axiom] - Case 3"))
  | Implies (_, And(_, Not(pNot, Or(_, And(_, Top _, And(_, In(pIn,(v,t),a), Top _)), Bottom _)), a1), And(_, Equals(_,l,r), Top _)) ->
     (let c = Not(pNot, In(pIn,(v,t),a)) in
      let data = collect a1 data in
      try create_rule (local_curry l data) (local_curry r data), Cond (local_curry c data), 42
      with _ -> raise (InternalError "Function [Axiom.of_implies_axiom] - Case 4"))
(* An example of the previous case:
  axiom{R} \implies{R} (
    \and{R} (
      \not{R} (
        \or{R} (
            \and{R} (
              \top{R}(),
              \and{R} (
                \in{SortInt{}, R} (
                  X0:SortInt{},
                  \dv{SortInt{}}("0")
                ),
                \top{R} ()
              )
          ),
          \bottom{R}()
        )
      ),
      \and{R}(
        \top{R}(),
        \and{R} (
          \in{SortInt{}, R} (
            X0:SortInt{},
            Var'Unds'0:SortInt{}
          ),
          \top{R} ()
        )
  ))
 [...]
 *)
  | _ -> raise (NotYetImplemented "Need to update [Axiom.of_implies_axiom].")
