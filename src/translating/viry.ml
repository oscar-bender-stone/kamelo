
(** This file generates the variante of Viry's transformation from
    Conditional Term Rewriting System (CTRS) to Term Rewriting System (TRS).

    Our tool performs two translations from CTRS to TRS, depending on
    whether the condition is formed with the predicate [isKResult],
    translation done in the file [eval_strategy.ml], or another predicate,
    translation performed here. *)

open Common.Xlib_OCaml
open Common.Error
open LP.Syntax
open Interface.LP_p_term
open Interface.K_prelude
open Interface.Getter_term
open Interface.Signature
open Axiom

(** Encoding on an example
    ----------------------

    Consider the following system:
      (1) rule max X Y => Y requires X <Int Y
      (2) rule max X Y => X requires X >=Int Y
    The encoding allows us to obtain the following TRS in Dedukti:
      (0)   rule max  $x $y          ↪ ♭max $x $y ♭ ♭
      (1')  rule ♭max $x $y ♭ $c     ↪ ♭max $x $y ($x < $y) $c
      (1'') rule ♭max $x $y true $c  ↪ $y
      (2')  rule ♭max $x $y $c ♭     ↪ ♭max $x $y $c ($x >= $y)
      (2'') rule ♭max $x $y $c true  ↪ $x

    The general idea of the encoding, inspired by Viry encoding is to add,
    for a symbol defined with conditional rules, as many arguments as there
    are conditions.

    Rule (0) rewrites a term whose heading symbol is [max], with a term using
    the corresponding extended version of arity 4, [♭max], where all boolean
    arguments are [♭], indicating that the boolean arguments have not yet
    been initialized by a condition.

    Rules (1') and (2') initialise the conditions to be calculated,
    while rules (1'') and (2'') reduce the size of the term
    since one of the conditions has been evaluated to true. *)

(** Formalisation of encoding
    -------------------------

 Assumptions:
   - To avoid naming conflicts, we assume that [♭] is an unused symbol name,
     and that it does not appear at the beginning of any symbol name.
   - The algorithm presented below takes as argument a set E_DK of triplets
     of Dedukti terms of the form (LHS, RHS, c), denoted LHS ↪c RHS. *)

(** A supposed safe prefix, i.e. there is no name beginning with it. *)
let safe_prefix = "♭"

(** The term ♭ *)
let p_FLAT = create_ident safe_prefix

(** The name ♭Bool *)
let _flatBool = safe_prefix ^ "Bool"

(** The term δ ♭Bool *)
let p_flatBool = p_INJD_appl_ident _flatBool

(** The name ♭inj *)
let _flatINJ = safe_prefix ^ _INJ

(** The term ♭inj *)
let p_flatINJ = create_ident _flatINJ

(** [p_flatINJ_appl s] creates the term ♭inj s. *)
let p_flatINJ_appl : p_term -> p_term = fun t -> create_appl p_flatINJ t

(** Notations:
   - head_<k> (See function [get_head_symbol]):
       Function that returns the heading symbol of the cell <k>, for a
       given rule, without considering the symbols [dotk], [kseq] and [inj].
       This is the heading symbol definition that we consider for a given
       rewrite rule.
   - C_σ (See type [equiv_class]):
       Set of rules that share the same heading symbol σ, i.e.
       C_σ = { l ↪c r | head_<k> (l ↪c r) = σ }.
   - X: Number of conditional rules in C_σ.
   - t_1[t_2]_σ (See function [update_config]):
       Substitution of the subterm with the heading symbol σ in [t_1],
       by [t_2].
   - arg_i(t): i-th argument of [t].
   - arity(t): Number of arguments of [t].
   - mglhs_σ (See function [create_most_general_LHS]):
       Transforms the initial configuration where the deepest cells become
         <c> y_i                                   if <c> ≠ <k>,
	     <k> (kseq (inj σ  z_1 ... z_(arity(σ)) L) otherwise,
       with y_i and z_i are fresh variables and, L is a K computation.
   - update_diff(σ, ♭σ, s_1, i, s_2)  (See function [with_one_diff_value]):
        = ♭σ  x_1  ...  x_(arity(σ) + X)
     with x_j =
		arg_j(σ)   if  1 <= j <= arity(σ)
		s_1        if  j = arity(σ) + i
		s_2        otherwise
   - update_same(σ, ♭σ, s)  (See function [with_all_same_value]):
        = ♭σ  x_1  ...  x_(arity(σ) + X)
     with x_j =
	 	arg_j(σ)    if  1 <= j <= arity(σ)
		s           otherwise

 Algorithm (See function [viry_encoding]):
   After constructing the C_σ from E_DK, we run the algorithm below,
   for each C_σ:
     [1.] If X = 0, C_σ is unchanged and the algorithm stops.
          Otherwise, initialise i to 0 and go to 2.
     [2.] Generate the most general LHS for a given symbol σ, denoted mglhs_σ.
     [3.] Generate the extended symbol ♭σ of type
            T_1 -> ... -> T_{n-1} -> ♭Bool -> ... -> ♭Bool -> T_n,
          with X argument(s) of type ♭Bool, where ♭Bool = Bool ∪ { ♭ },
          and σ of type T_1 -> ... -> T_n.
          See function [extend_type].
	 [4.] Generate the substitution rule:
            mglhs_σ ↪ mglhs_σ [ update_same(σ, ♭σ, ♭) ]_σ
          See function [create_substitution_rule].
	 [5.] For each rule belonging to C_σ:
           (a) If the rule is of the form l ↪c r ∈ C_σ
               and does not have the [owise] attribute (with c ≠ ⊤):
                 - Increment i by 1
	             - Generate an initialization rule:
                     l [ update_diff(σ, ♭σ, ♭, i, _) ]_σ ↪
		             l [ update_diff(σ, ♭σ, c, i, _) ]_σ
                   See function [create_initialization_rule].
   	             - Generate a reduction rule :
		             l [ update_diff(σ, ♭σ, true, i, _) ]_σ ↪ r
                   See function [create_reduction_rule].

           (b) If the rule is of the form l ↪⊤ r ∈ C_σ and does not have
               the [owise] attribute, generate the reduction rule:
                  l [ update_same(σ, ♭σ, _) ]_σ ↪ r.
               See function [create_reduction_rule].

           (c) If the rule l ↪⊤ r ∈ C_σ has the [owise] attribute,
               generate the reduction rule :
	  	          l [ update_same(σ, ♭σ, false) ]_σ ↪ r.
               See function [create_otherwise_rule]. *)

(** Extend the encoding with the [owise] attribute
    ----------------------------------------------

    A easiest way to write the above example is to use the [owise] attribute:
      rule max X Y => Y requires X <Int Y
      rule max X Y => X [owise]
    Since K does not generate the complementary condition in Kore file,
    we assume that any function that returns a boolean is a total function.
    Under this assumption, we can generate the rule presented in 5.(c). *)


(** ------------------------------ *)
(** To create each C_σ from a CTRS *)
(** ------------------------------ *)

(** Type of equivalence class related to the head symbol named σ,
    i.e. a map where each entry has the form
    σ |-> [((LHS, RHS), Some condition, priority) ; ...].
    For a head symbol σ given, C_σ is the corresponding equivalence class. *)
type equiv_class = (ctrs_rule list) StrMap.t

(** [is_cell s] returns true if a string [s] is a cell's name. *)
let is_cell : string -> bool = fun s ->
  let len = String.length s in
  try
    if !Interface.Output.readable
    then s.[0] = '<' && s.[len-1] = '>'
    else
      String.sub s 0 9 = "Lbl'-LT-'"
      && String.sub s ((String.length s)-6) 6 = "'-GT-'"
  with _ -> false

(** [is_to_keep s] returns true if a string [s] is [dotk], [kseq] or [inj]. *)
let is_to_keep : string -> bool = fun s ->
  s = _KSEQ || s = _DOTK || s = _INJ

(** [get_head_symbol config] is the function head_<k>, i.e. returns:
      - None    if no configuration
      - Some h  otherwise, where h is the heading symbol of the cell <k>.
    The symbols [dotk], [kseq] and [inj] are not considered to be
    a heading symbol. *)
let get_head_symbol config =
  let rec aux : p_term -> string option = fun t ->
    match t.elt with
    | P_Appl(t1, t2) ->
       (match aux t1 with
        | None   -> aux t2
        | Some x -> Some x)
    | P_Patt _ -> None
    | P_Expl _ -> None
    | P_Iden (name, _) ->
       let n = snd name.elt in
       if is_to_keep n then None
       else (if is_cell n then None else Some n)
    | _ -> (* raise (InternalError "The function [Viry.get_head_symbol] need to be fixed.") *)
       wrn_msg _STDOUT "FIXME" ; None
  in
  aux config

(* TODO used it?   exception KCellNotFound   exception KCellNotFoundHere *)
(** [find_equiv_class ec r] adds the condition rule [r] into the equivalence
    class [ec].  *)
let find_equiv_class : equiv_class -> ctrs_rule -> equiv_class =
  fun ec (({elt=(lhs,_);_},_,_) as r) ->
  match get_head_symbol lhs with
  | None -> ec
  | Some key -> add_update key r ec

(*
  let key = match get_head_symbol lhs with
    | None   -> raise (InternalError "The function [Viry.find_equiv_class] need to be fixed.")
    (* raise  KCellNotFound *)
    | Some x -> x
  in
  let f ec = try key with _ -> wrn_msg _STDOUT "FIXME" ; ec in
  add_update f r ec *)

(** [to_equiv_class rule_l] generates each equivalence class from
    a CTRS [rule_l], i.e. generates each C_σ. *)
let to_equiv_class : ctrs_rule list -> equiv_class = fun rule_l ->
  List.fold_left find_equiv_class StrMap.empty rule_l

(** ----------------------------- *)
(** To iterate on a configuration *)
(** ----------------------------- *)

(** [has_infered_configuration t] returns true if the term [t]
    is composed of cells, i.e. the configuration has been infered
    during the translation from K to Kore. *) (* TODO correct ? *)
let rec has_infered_configuration : p_term -> bool = fun t ->
  match t.elt with
  | P_Appl(t,_)    -> has_infered_configuration t
  | P_Iden(name,_) -> is_cell (snd name.elt)
  | _ -> false

(** [update_config head config f] applies the function [f] to the subterm with
    the heading symbol [head] in the configuration [config]. *)
let update_config : string -> p_term -> (p_term -> p_term) -> p_term =
  fun head config f ->
  let rec aux : bool -> p_term -> bool * p_term = fun is_head t ->
    match t.elt with
    | P_Appl(t1, t2) ->
       (let l_is_head, x1 = aux is_head t1 in
        let r_is_head, x2 = aux is_head t2 in
        if r_is_head then
          (* (if l_is_head TODO Correcte ?
           then raise (KaMeLoError (InternalError, "Viry", "update_config", "Several head symbols..."))
           else *) false, create_appl x1 (f x2)
        else l_is_head, create_appl x1 x2)
    | P_Patt _ | P_Expl _ -> false, t
    | P_Iden(({elt=(x1,n);pos=y}), x2) ->
       if n = head
       then true,  no_pos (P_Iden(({elt=(x1, safe_prefix ^ n);pos=y}), x2))
       else false, t
    | _ -> raise (KaMeLoError (InternalError, "Viry", "has_infered_configuration", ""))
  in
  let res = snd(aux false config) in
  if has_infered_configuration config then res else f res

(** ----------------------------------------------- *)
(** To generate the most general LHS for a symbol σ *)
(** ----------------------------------------------- *)

(** [is_k_cell s] returns true if a string [s] is the cell's name
    of the cell k. *)
let is_k_cell : string -> bool = fun s ->
  s = Interface.Output.pp _K_CELL

(** [create_most_general_LHS t] transforms the initial configuration [t]
    as follow:
       <c> y_i                                   if <c> ≠ <k>,
	   <k> (kseq (inj σ z_1 ... z_(arity(σ)) L)  otherwise,
    where y_i and z_i are fresh variables and, L is a K computation.
    The result is denoted mglhs_σ. *) (* TODO improve *)
let create_most_general_LHS t =
  let nb = ref 0 in (* TODO remove ? *)
  let new_var nb =
    incr nb ; create_pattern_var ("x" ^ string_of_int !nb) (* TODO y ? *)
  in
  let rec aux : p_term -> bool -> bool -> bool * bool * bool * p_term =
    fun t is_in_k_cell is_head ->
    match t.elt with
    | P_Appl(t1, t2) ->
       (let l_is_in_k_cell, l_merged, l_is_head, x1 =
          aux t1 is_in_k_cell is_head
        in
        if l_merged then is_in_k_cell, l_merged, l_is_head, new_var nb
        else
          let r_is_in_k_cell, r_merged, r_is_head, x2 =
            aux t2 l_is_in_k_cell l_is_head
          in
          let res =
            if r_merged then create_appl x1 (new_var nb)
            else create_appl x1 x2
          in
          is_in_k_cell, l_merged && r_merged, l_is_head || r_is_head, res)
    | P_Patt _ as t -> is_in_k_cell, true, is_head, no_pos t
    | P_Expl _ as t -> is_in_k_cell, not(is_in_k_cell), is_head, no_pos t
    | P_Iden (name, _) as t ->
       let n = snd name.elt in
       if is_to_keep n then
         is_in_k_cell, not(is_in_k_cell), is_head, no_pos t
       else
         (if is_cell n then
            (is_k_cell n), false, is_head, no_pos t
          else
            if is_head
            then is_in_k_cell, true,  is_head, no_pos t
            else is_in_k_cell, false, true,    no_pos t)
    | _ -> raise (KaMeLoError (InternalError, "Viry", "create_most_general_LHS", ""))
  in
  let _,_,_,res = aux t (not(has_infered_configuration t)) false in res

(** --------------------------------------------------- *)
(** To generate the ♭-symbol of the current head symbol *)
(** --------------------------------------------------- *)

(** [create_list n default_sym i special_sym] creates a list with
    [n] symbols [default_sym], except the [i]e which is [special_sym].
    More precisely:
     - 0 <= n ==> List.length @result = n
     - n <= 0 ==> List.length @result = 0
     - \forall k. k <> i /\ 0 <= k < n ==> @result.[k] = default_sym
     - 0 <= i < n ==> @result.[i] = special_sym
     - Remark: n <= i ==> (\forall k. 0 <= k < n ==> @result.[k] = default_sym) *)
let create_list n default_sym i special_sym =
  if n <= 0 then []
  else
    let rec aux current =
      let adding_sym = if current = i then special_sym else default_sym in
      if current = (n-1) then
        [adding_sym]
      else
        adding_sym::(aux (current+1))
    in
    aux 0

(** [create_list_iter nb default_sym] creates a list, thanks to [create_list],
    with [nb] symbols [default_sym]. *)
let create_list_iter nb default_sym =
  (* nb < nb+1, so the list has only [default_sym] element *)
  create_list nb default_sym (nb+1) default_sym

(** [extend_type typ nb] generates the type:
      T_1 -> ... -> T_{n-1} -> ♭Bool -> ... -> ♭Bool -> T_n,
    with [typ] is T_1 -> ... -> T_n and [nb] argument(s) of type ♭Bool,
    where ♭Bool = Bool ∪ { ♭ }. *)
let extend_type typ nb =
  let split_type : p_term -> p_term list * p_term = fun typ ->
    let rec aux (t : p_term) acc = match t.elt with
      | P_Type   -> [], t
      | P_Iden _ -> [], t
      | P_Appl _ -> [], t
      | P_Arro(t1, ({elt=P_Type  ;_} as t2)) -> List.rev (t1::acc), t2
      | P_Arro(t1, ({elt=P_Iden _;_} as t2)) -> List.rev (t1::acc), t2
      | P_Arro(t1, ({elt=P_Appl _;_} as t2)) -> List.rev (t1::acc), t2
      | P_Arro(t1, ({elt=P_Arro _;_} as t2)) -> aux t2 (t1::acc)
      | _ -> raise (KaMeLoError (InternalError, "Viry", "extend_type", "Unexpected type which to be extended during Viry's transformation."))
    in
    aux typ []
  in
  let arg_type, output_type = split_type typ in
  List.fold_right create_arrow (arg_type@(create_list_iter nb p_flatBool)) output_type

(** --------------------------- *)
(** To generate rewriting rules *)
(** --------------------------- *)

(** [with_one_diff_value heading nb i special_sym] creates left- or
    right-hand-side of the form:
      [heading] _ ... _ [special_sym] _ ... _
    where [heading] has [nb] argument(s),
    and [special_sym] are only at the position [i].
    Note: Equivalent to update_diff(σ, ♭σ, special_sym, i, _)
          where [heading] = ♭σ, and [nb] = arity(heading). *)
let with_one_diff_value heading nb i special_sym =
  List.fold_left create_appl heading (create_list nb p_WILD i special_sym)

(** [with_all_same_value heading nb default_sym] creates left- or
    right-hand-side of the form:
      [heading] [default_sym] ... [default_sym]
    where [heading] has [nb] argument(s).
    Note: Equivalent to update_same(σ, ♭σ, default_sym)
          where [heading] = ♭σ, and [nb] = arity(heading). *)
let with_all_same_value heading nb default_sym =
  List.fold_left create_appl heading (create_list_iter nb default_sym)

(** [create_substitution_rule tracker config nb]
    creates an substitution rule, i.e. a rule of the form:
      rule config ↪ config [ update_same(σ, ♭σ, ♭) ]_σ
    where [nb] occurrence(s) of ♭.
    Note: [tracker] = [update_config ♭σ]. *)
let create_substitution_rule tracker config nb : p_rule =
  let f h = with_all_same_value h nb p_FLAT in
  create_rule config (tracker config f)

let create_list_number n i special_sym =
  if n <= 0 then []
  else
    let rec aux current =
      let adding_sym =
        if current = i then special_sym
        else create_pattern_var ("y" ^ (string_of_int i)) in
      if current = (n-1) then [adding_sym] else adding_sym::(aux (current+1))
    in
    aux 0

(** [create_initialization_rule tracker nb i special_sym_l special_sym_r]
    creates an initialization rule, i.e. a rule of the form:
      rule l [ update_diff(σ, ♭σ, special_sym_l, i, _) ]_σ ↪
           l [ update_diff(σ, ♭σ, special_sym_r, i, _) ]_σ
    where ♭σ has [nb] argument(s),
    and [special_sym_l] and [special_sym_r] only occur at the position [i].
    Note: [tracker] = [update_config ♭σ l]. *)
let create_initialization_rule tracker nb i special_sym_l special_sym_r : p_rule =
  let f special_sym h =
    List.fold_left create_appl h (create_list_number nb i special_sym)
  in
  create_rule (tracker (f special_sym_l)) (tracker (f special_sym_r))

(** [create_reduction_rule tracker nb i special_sym rhs]
    creates a reduction rule, i.e. a rule of the form:
      rule l [ update_diff(σ, ♭σ, special_sym, i, _) ]_σ ↪ [rhs]
    where ♭σ has [nb] argument(s),
    and [special_sym] only occurs at the position [i].
    Note: [tracker] = [update_config ♭σ l]. *)
let create_reduction_rule tracker nb i special_sym rhs : p_rule =
  let f h = with_one_diff_value h nb i special_sym in
  create_rule (tracker f) rhs

(** [create_otherwise_rule tracker nb rhs]
    creates a reduction rule, i.e. a rule of the form:
      rule l [ update_same(σ, ♭σ, false) ]_σ ↪ [rhs].
    where [nb] occurrence(s) of "false".
    Note: [tracker] = [update_config ♭σ l]. *)
let create_otherwise_rule tracker nb rhs : p_rule =
  let f h =
    with_all_same_value h nb (p_flatINJ_appl p_FALSE)
  in
  create_rule (tracker f) rhs

(** ------------------ *)
(** The main algorithm *)
(** ------------------ *)

let viry_encoding : ctrs_rule list -> signature -> p_symbol list * p_rule list = fun l sign ->
  (* [0.] Create the initial data (♭Bool, ♭, ♭inj, and each C_σ). *)
     (* [a.] Create the symbol ♭Bool. *)
  let flat_bool_sym = create_p_symbol [] _flatBool [] (Some p_SORTK) None in
     (* [b.] Create the symbol ♭. *)
  let flat_sym = create_p_symbol [] safe_prefix [] (Some p_flatBool) None in
     (* [c.] Create the symbol ♭inj. *)
  let flat_inj_type = create_arrow (p_INJD_appl_ident _SORT_BOOL) p_flatBool in
  (* δ SortBool → δ ♭Bool *)
  let flat_inj_sym = create_p_symbol [] _flatINJ [] (Some flat_inj_type) None in
     (* [d]. Create each C_σ from a CTRS. *)
  let equiv_class = to_equiv_class l in
  (* For each C_σ *)
  let aux_sigma : string -> ctrs_rule list -> p_symbol list * p_rule list -> p_symbol list * p_rule list =
    fun head_name l (acc_sym, acc_rule) ->
    let tracker = update_config head_name in
    (* [1.] Compute the number of conditions in C_σ, in order to know
            if there is no conditional rule. *)
    let nb_cond = List.fold_left (fun acc (_,data,_) -> match data with | Cond _ -> 1 + acc | _ -> 0 + acc) 0 l in
    if nb_cond = 0
    then acc_sym, List.map (fun (pr,_,_) -> pr) l@acc_rule
    else
      (* [2.] Generate the most general LHS for a given head symbol σ. *)
      let (pr, _, _) = List.hd l in (* TODO FIX if the list is empty *)
      let mglhs = create_most_general_LHS (fst pr.elt) in
      (* [3.] Generate the ♭-symbol of the current head symbol. *)
      let flat_head_name = safe_prefix ^ head_name in
      let flat_head_type =
        try
          extend_type (StrMap.find head_name sign.typing) nb_cond
        with Not_found -> raise (KaMeLoError (InternalError, "Viry", "viry_encoding", ("The symbol " ^ head_name ^ " wasn't defined.")))
      in
      let flat_head_sym =
        create_p_symbol [] flat_head_name [] (Some flat_head_type) None
      in
      (* [4.] Generate the substitution rule. *)
      let encap_r = create_substitution_rule tracker mglhs nb_cond in
      (* [5.] For each rule in C_σ *)
      let rec aux_rule : int -> p_rule list -> ctrs_rule list -> p_rule list = (* ~ fold_lefti *)
        fun i acc ctrs_l ->
        match ctrs_l with
        | [] -> acc
        (* [a.] If the rule is conditional. *)
        | ({elt=(lhs,rhs);_}, Cond c, _)::q ->
           let curr_tracker = tracker lhs in
           let init_r =
             create_initialization_rule curr_tracker nb_cond i p_FLAT (p_flatINJ_appl c)
           in
           let reduc_r =
             create_reduction_rule curr_tracker nb_cond i
                       (p_flatINJ_appl p_TRUE) rhs
           in
           aux_rule (i+1) (init_r::reduc_r::acc) q
        (* [b.] If the rule is unconditional. *)
        | ({elt=(lhs,rhs);_}, Uncond, _)::q ->
           let reduc_r =
             create_reduction_rule (tracker lhs) nb_cond i p_WILD rhs
           in
           aux_rule i (reduc_r::acc) q
        (* [c.] If the rule has the [owise] attribut. *)
        | ({elt=(lhs,rhs);_}, OwiseRule, _)::q ->
           let owise_r = create_otherwise_rule (tracker lhs) nb_cond rhs in
           aux_rule i (owise_r::acc) q
      in
      let new_acc_sym =
        if List.mem flat_head_sym acc_sym then acc_sym
        else flat_head_sym::acc_sym
      in
      new_acc_sym, aux_rule 0 [encap_r] l@acc_rule
  in
  let f head_name l (acc_sym, acc_rule) =
    try aux_sigma head_name l (acc_sym, acc_rule)
    with (* KaMeLoError(t, fileN, funcN, msg) -> (* TODO *)
      wrn_no_translation (t, fileN, funcN, msg) pos *)
        _ -> wrn_msg _STDOUT "FATAL VIRY" ; (acc_sym, acc_rule)
  in
  StrMap.fold f equiv_class ([flat_inj_sym;flat_sym;flat_bool_sym], [])
