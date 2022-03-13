
open LP.Syntax

open Common.Type
open Common.Error
open Common.Xlib_OCaml

open Interface.LP_p_term
open Interface.K_prelude
open Interface.Signature

open Mecanism.Axiom_iterator

(** ------------------------------------------ *)
(** Some meta-functions to iterate on axiom    *)
(** ------------------------------------------ *)

let sym_case : name * param list * p_term list -> 's -> 'd -> p_term * 's * 'd =
  fun (n, qv_l, a_l) sign data ->
  let a_l = List.rev a_l in
  (if n = _INJ then
     let g p = match p with S x | Q x -> create_implicit_arg x in
     let tmp = List.map g qv_l in
     let res = List.fold_left create_appl p_INJ tmp in
     List.fold_left create_appl res a_l
   else
     List.fold_left create_appl (create_ident n) a_l), sign, data

(** [var_case f (n, _) s d] uses local data [d] to replace the variable [n] by a pattern. *)
let var_case : (name -> p_term) -> name * param -> 's -> p_term StrMap.t -> p_term * 's * p_term StrMap.t = fun f (n, _) s d ->
    (if StrMap.mem n d then StrMap.find n d else f n), s, d

let iter_meta :
   (param list * 'r * sort * name    -> 's -> 'd -> 'r * 's * 'd) ->
   (string -> p_term) -> axiom -> signature -> p_term StrMap.t -> p_term * p_term StrMap.t =
  fun f_equals_dom_val f_var ax sign_init local_data_init ->
  let f_predicate_sym = sym_case in
  let f_predicate_var (n, p) s d = var_case f_var (n, p) s d in
  let f_dom_val (_, name) s d = create_ident name, s, d in
  let f_not _ _ _ =
    raise (NotYetImplemented "Need to update [Axiom.iter_meta] - Case not")    in
  let f_not_in _ _ _ =
    raise (NotYetImplemented "Need to update [Axiom.iter_meta] - Case not-in") in
  let f_equals _ _ _ =
    raise (NotYetImplemented "Need to update [Axiom.iter_meta] - Case equals") in
  let f_and _ _ _ =
    raise (NotYetImplemented "Need to update [Axiom.iter_meta] - Case and")    in
  let f_and_var (_, n, _, ax) s d = ax, s, StrMap.add n ax d in
  let res, _, local_data_res =
    axiom_iter_default_error [] ax f_var sign_init local_data_init
      f_predicate_sym f_predicate_var f_dom_val
      f_not f_not_in f_equals f_equals_dom_val f_and f_and_var
  in res, local_data_res

(** --------------------------------------- *)
(** Common functions to iterate on axiom    *)
(** --------------------------------------- *)

let iter_axiom : (string -> p_term) -> axiom -> signature -> p_term StrMap.t -> p_term * p_term StrMap.t =
  fun f_var ax sign local_data ->
  let f_equals_dom_val _ _ _ =
    raise (NotYetImplemented "Need to update [Axiom.iter_axiom] - Case equals-dom_val") in
  iter_meta f_equals_dom_val f_var ax sign local_data

let iter_to_ident   = iter_axiom create_ident
let iter_to_pattern = iter_axiom create_pattern_var

(** ------------------------------------------------------ *)
(** To translate exists-axioms (functional or subsort one) *)
(** ------------------------------------------------------ *)

(** Currently, functional axioms aren't used and
    subsort axioms are just used to change some injections. *)

(** [collect_subsort_data ax sign] updates the relations of subsorts in the
    signature [sign].
    For instance, if this function matches an axiom with the following shape:
      \exists{R} (Val:SortKItem{},
                  \equals{SortKItem{}, R}
                      (Val:SortKItem{},
                       inj{SortCell{}, SortKItem{}} (From:SortCell{})))
    then it adds the subsort relation SortCell <: SortKItem.
    Otherwise, it raises an error. *)
let collect_subsort_data : axiom -> signature -> signature = fun ax sign ->
  match ax with
  | Exists (_, _, Equals(_, _, Predicate(Sym(s, [S s1; S s2], _)))) when s = _INJ ->
     { sign with subsort = add_update s1 s2 sign.subsort }
  | _ -> raise (InternalError "Need to update [Axiom.collect_subsort_data].")

(** ---------------------------------------------------- *)
(** To translate equals-axioms
    (Associative, Commutative, Unit and Idempotence one) *)
(** ---------------------------------------------------- *)

let of_equality_axiom : axiom -> p_rule = fun ax -> (* TODO sign ?*)
  match ax with
  | Equals(_, ax1, ax2) ->
     (try
        let lhs, ld = iter_to_pattern ax1 empty_sign StrMap.empty in
        let rhs, ld = iter_to_pattern ax2 empty_sign ld in
        create_rule lhs rhs
      with _ -> raise (InternalError "Need to update [Axiom.of_equality_axiom]."))
  | _ -> raise (InternalError "The current axiom isn't an equality one.\n
                Please, raise an issue.")

(** ---------------------------------------------------- *)
(** To translate or-axioms, bottom-axioms, not-axioms
    (aka constructor one) *)
(** ---------------------------------------------------- *)

(** Currently, these axioms aren't used. *)

(** ------------------------------ *)
(** To translate implies-axioms    *)
(** ------------------------------ *)

(** Type of extra data about a rule *) (* TODO priority ? *)
type extra_data_rule =
 | Uncond         (* A uncondtional rule *)
 | Cond of p_term (* A conditional rule with a condition *)
 | OwiseRule      (* A rule with the attribut "owise" *)

(** Type of a rule in a CTRS, which has the form
    ((LHS, RHS), extra_data_rule, priority) *)
type ctrs_rule = p_rule * extra_data_rule * int

(* This is an example of the implies-axiom translation:
   axiom{R} \implies{R} (
   *   \and{R}(
   *     \top{R}(),          <-- Condition
   *     \and{R} (                           | local data:
   *         \in{SortBool{}, R} (            |   X0 = false
   *           X0:SortBool{},                |
   *           \dv{SortBool{}}("false")  <-- |
   *         ),\and{R} (                     |
   *         \in{SortBool{}, R} (            |   X1 = VarB
   *           X1:SortBool{},                |
   *           VarB:SortBool{}               |
   *         ),                              |
   *         \top{R} ()                      |
   *       ))),                              |
   *   \and{R} (
   *     \equals{SortBool{},R} (
   *       Lbl'Unds'orBool'Unds'{}(X0:SortBool{},X1:SortBool{}), <-- LHS
   *       VarB:SortBool{}),                                     <-- RHS
   *     \top{R}()))

 So, the rule is: false orBool VarB --> VarB *)

let iter_implies : (string -> p_term) -> axiom -> p_term StrMap.t -> p_term = fun f_var ax local_data_init ->
  let f_predicate_sym = sym_case in
  let f_predicate_var (n, p) s d = var_case f_var (n, p) s d in
  let f_dom_val (_, name) s d = create_ident name, s, d in
  let f_not_in (_, _, (v,_), a) s d =
    create_appl
      p_NOT_BOOL
      (create_appl
         (create_appl p_EQ_K (create_ident v)) (* TODO typage + quelle égalité ? *)
         a), s, d (* raise (NotYetImplemented "TODO") *)
  in     (* symbol eq : δ SortKItem → δ SortKItem → δ SortKItem;
           rule ♭Lblf'UndsUnds'FALSE-SYNTAX'Unds'Bool'Unds'Int $Var'Unds'0 ♭ ↪
                ♭Lblf'UndsUnds'FALSE-SYNTAX'Unds'Bool'Unds'Int $Var'Unds'0 (♭inj (LblnotBool'Unds' (inj (eq (inj $Var'Unds'0) (inj 0))))); *)
  let f_not _ _ _ =
      raise (NotYetImplemented "Need to update [Axiom.iter_implies] - Case not")      in (* TODO different! *)
  let f_equals _ _ _ =
    raise (NotYetImplemented "Need to update [Axiom.iter_implies] - Case equals")     in
  let f_equals_dom _ _ _ =
    raise (NotYetImplemented "Need to update [Axiom.iter_implies] - Case equals-dom") in
  let f_and _ _ _ =
      raise (NotYetImplemented "Need to update [Axiom.iter_implies] - Case and")      in (* TODO different! *)
  let f_and_var (_, _, _, ax) s d = ax, s, d in
  let res, _, _ =
    axiom_iter_default_error [] ax f_var StrMap.empty local_data_init
      f_predicate_sym f_predicate_var f_dom_val
      f_not f_not_in f_equals f_equals_dom f_and f_and_var
  in res

let iter_implies = iter_implies create_pattern_var

(** [collect ax acc] *)
let rec collect : axiom -> p_term StrMap.t -> p_term StrMap.t = fun ax acc ->
  match ax with
  | Top _ -> acc
  | In(_,(v1,_), And(_, Dom_val(x,"false"), Predicate(Var(v2, _))))
    | In(_,(v1,_), And(_, Predicate(Var(v2, _)), Dom_val(x,"false"))) ->
     let tmp = StrMap.add v1 (iter_implies (Dom_val(x,"false")) acc) acc in
     StrMap.add v2 (iter_implies (Dom_val(x,"false")) tmp) tmp
  | In(_,(v,_),a) -> StrMap.add v (iter_implies a acc) acc
  | And(_,Top _,ax) | And(_,ax,Top _) -> collect ax acc
  | And(_, ax1, ax2) -> collect ax2 (collect ax1 acc)
  | _ -> raise (NotYetImplemented "Need to update [Axiom.collect].")

(** [of_implies_axiom ax] translates the axiom [ax] which begins by "\implies"
    to a rewriting rule. *)
let of_implies_axiom : axiom -> ctrs_rule = fun ax ->
  let init_data = StrMap.empty in
  match ax with
  | Implies(_, And(_,Top _, a1), And(_, Equals(_,l, r), Top _)) ->
     (let data = collect a1 init_data in
      try create_rule (iter_implies l data) (iter_implies r data), Uncond, 42
      with _ -> raise (InternalError "Function [Axiom.of_implies_axiom] - Case 1"))
  | Implies(_, And(_, Equals(_, c, Dom_val(_,"true")), a1), And(_, Equals(_,l, r), Top _)) ->
     (let data = collect a1 init_data in
      try create_rule (iter_implies l data) (iter_implies r data), Cond (iter_implies c data), 42 (* TODO iter_condition ? *)
      with _ -> raise (InternalError "Function [Axiom.of_implies_axiom] - Case 2"))
  | Implies(_, Equals(_, c, Dom_val(_,"true")), And(_, Equals(_,l, r), Top _)) ->
      (try create_rule (iter_implies l init_data) (iter_implies r init_data), Cond (iter_implies c init_data), 42 (* TODO iter_condition ? *)
       with _ -> raise (InternalError "Function [Axiom.of_implies_axiom] - Case 3"))
  | Implies (_, And(_, Not(pNot, Or(_, And(_, Top _, And(_, In(pIn,(v,t),a), Top _)), Bottom _)), a1), And(_, Equals(_,l, r), Top _)) ->
     (let c = Not(pNot, In(pIn,(v,t),a)) in
      let data = collect a1 init_data in
      try create_rule (iter_implies l data) (iter_implies r data), Cond (iter_implies c data), 42 (* TODO iter_condition ? *)
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
 [...] *)
  | _ -> raise (NotYetImplemented "Need to update [Axiom.of_implies_axiom].")
