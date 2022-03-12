(** To translate heating and cooling rules, i.e. evaluation strategies
    For example:
      rule E1 and E2               => E1 ~> (freezer1_and E2) requires not(E1 ∈ KResult) (règle C) and
      rule E1 ~> (freezer1_and E2) => E1 and E2               requires E1 ∈ KResult      (règle H)     *)

open LP.Syntax

open Common.Type
open Common.Getter
open Common.Error
open Common.Xlib_OCaml

open Interface.LP_p_term
open Interface.K_prelude
open Interface.Signature
open Interface.Output

open Axiom

(** ---------------------------- *)
(** Common functions             *)
(** ---------------------------- *)

(** To do substitution *)

(** [subst t a name] replaces each variable named [name], by [a] in [t]. *)
let subst t a name =
  let rec aux : p_term -> p_term = fun t ->
    match t with
    | ({elt=P_Appl(t1, t2);pos=p}) -> {elt=P_Appl(aux t1, aux t2);pos=p}
    | ({elt=P_Patt(Some {elt=x;pos=_}, _);pos=_}) ->
       if x = name then a else t
    | t -> t
  in
  aux t

(** [subst_sort t new_s name] replaces the sort of each variable named [name], by [new_s] in [t]. *)
let subst_sort (t : p_term) new_s name : p_term =
  let rec aux : p_term_aux -> p_term_aux = fun t ->
    match t with
    (* If t has the shape: _INJ {s1} {s2} (name), replaces it by _INJ {new_s} {s2} (name) *)
    | P_Appl(
        {elt=P_Appl(
                 {elt=P_Appl({elt=P_Iden({elt=(x1, s);pos=x2}, x3);pos=x4},
                             {elt=P_Expl({elt=P_Iden ({elt=(x5,s1) ;pos=x6}, x7); pos=x8}) ; pos=x9} )
                 ; pos=x10},
                 {elt=P_Expl({elt=P_Iden ({elt=(x11,s2) ;pos=x12}, x13); pos=x14}) ; pos=x15} )
        ; pos=x16},
        {elt=P_Patt(Some {elt=n ;pos=x17}, x18) ; pos=x19} ) when s = _INJ && n = name ->
       P_Appl(
           {elt=P_Appl(
                    {elt=P_Appl({elt=P_Iden({elt=(x1, s);pos=x2}, x3);pos=x4},
                                {elt=P_Expl({elt=P_Iden ({elt=(x5,new_s) ;pos=x6}, x7); pos=x8}) ; pos=x9} )
                    ; pos=x10},
                    {elt=P_Expl({elt=P_Iden ({elt=(x11,s2) ;pos=x12}, x13); pos=x14}) ; pos=x15} )
           ; pos=x16},
           {elt=P_Patt(Some {elt=n ;pos=x17}, x18) ; pos=x19} )

    | P_Appl(({elt=t1;pos=x1}), ({elt=t2 ;pos=x2})) ->
       P_Appl(({elt=aux t1;pos=x1}), ({elt=aux t2 ;pos=x2}))
    | _ -> t
  in
  {elt=aux t.elt ; pos= t.pos}

(** To create a specific Dedukti term *)

(** [create_inj_var input_sort output_sort var_name] creates the Dedukti term
    [_INJ {new_sort} {sort_output} (var_name)]. *)
let create_inj_var : string -> string -> string -> p_term =
  fun input_sort output_sort var_name ->
  {elt=P_Appl(
        {elt=P_Appl(
              {elt=P_Appl({elt=P_Iden({elt=([], _INJ);pos=None}, false);pos=None},
                      {elt=P_Expl({elt=P_Iden ({elt=([], input_sort) ;pos=None}, false); pos=None}) ; pos=None} )
              ; pos=None},
              {elt=P_Expl({elt=P_Iden ({elt=([], output_sort) ;pos=None}, false); pos=None}) ; pos=None} )
        ; pos=None},
        {elt=P_Patt(Some {elt=var_name ;pos=None}, None) ; pos=None} )
  ; pos=None}

(** To iterate on axiom *)

let curry_condition : (string -> p_term) -> axiom -> signature -> p_term StrMap.t -> p_term = fun f_var ax sign local_data ->
  let f_equals_dom_val (p_l, x, s, dom) s d =
    (if dom = _TRUE then x
     else
       if dom = _FALSE then create_appl (create_ident _NOT_BOOL) x
       else raise (NotYetImplemented "Need to update [Axiom.curry_condition] - Case equals-dom_val")), s, d
  in
  fst (curry_meta f_equals_dom_val f_var ax sign local_data)

let curry_condition = curry_condition create_pattern_var

let create_LHS : alias -> signature -> p_term * p_term StrMap.t * p_term option = fun al sign ->
  let get_def : alias -> def = fun (_,(_,_,_,def)) -> def in
  let def = get_def al in
  match def with
  | A a ->
     begin
       match a with
       | And(_,a1,a2) ->
          (match a1 with
           | Top _ -> let res, local_data = curry_pattern a2 sign StrMap.empty in
                      res, local_data, None
           | _     -> let res, local_data = curry_pattern a2 sign StrMap.empty in res, local_data, Some (curry_condition a1 sign local_data))
       |  _ -> raise (InternalError "The heating/cooling rule has no condition")
     end
  | D _ -> raise (NotYetImplemented "Alias (LHS) with a unique symbol as body.")

let create_RHS : axiom -> signature -> p_term StrMap.t -> p_term = fun ax sign local_data ->
  match ax with
  | Rewrites(_,_,And(_,a1,a2)) ->
     if is_conditional_rule a1 then
       raise (NotYetImplemented "KProver claim.")
     else
       let res, _ = Axiom.curry_pattern a2 sign local_data in res
  |  _ -> raise (InternalError "The heating/cooling rule doesn't begin with \rewrites.")

(** ---------------------------- *)
(** To translate heating rules   *) (* For now, its a cooling rule... *)
(** ---------------------------- *)

exception Not_In

(** [get_var_and_sort_inj cond sign] returns the variable in the condition [cond]
    with its input sort and output sort. *) (* TODO update when there are several variables *)
let get_var_and_sort_inj : p_term -> signature -> string * string * string = fun cond sign ->
  let rec aux t = match t with
    (* If t has the shape: _INJ {s1} {s2} ($HOLE) *)
    | P_Appl(
        {elt=P_Appl(
            {elt=P_Appl({elt=P_Iden({elt=(x1, s);pos=x2}, x3);pos=x4},
                        {elt=P_Expl({elt=P_Iden ({elt=(x5,s1) ;pos=x6}, x7); pos=x8}) ; pos=x9} )
            ; pos=x10},
            {elt=P_Expl({elt=P_Iden ({elt=(x11,s2) ;pos=x12}, x13); pos=x14}) ; pos=x15} )
        ; pos=x16},
        {elt=P_Patt(Some {elt=("HOLE" as n) ;pos=x17}, x18) ; pos=x19} ) when s = _INJ -> n, s1, s2 (* TODO remove HOLE *)

    | P_Appl(({elt=t1;pos=x1}), ({elt=t2 ;pos=x2})) ->
       (try aux t1
        with Not_In -> aux t2)
    | P_Iden _ | P_Expl _ -> raise Not_In
    | t -> aux t
  in
  aux cond.elt

let trans_heating_rule : attribute list -> ctrs_rule list -> signature -> alias -> quant_var list * axiom -> ctrs_rule list =
  fun attr_l acc sign al (_, ax) ->
  (* To understand this algorithm, consider the following example:
          rule E1 ~> (freezer1_and E2) => E1 and E2 requires E1 ∈ KResult (règle H) *)

  (* STEP 0: *)
  (* Be careful: the order of the computation is important
     because of references *)
  let default_prio = 42 in
  let lhs, local_data, cond = create_LHS al sign in
  let rhs = create_RHS ax sign local_data in

  let cond = match cond with | None -> raise (InternalError "No condition for a heating rule.") | Some x -> x in

  (* STEP 1: Get the variable in the condition with its type (here "E1" and "BExp")  *)
  let var_name, sort_input, sort_output = get_var_and_sort_inj cond sign in

  (* STEP 2: Compute the subsort that defines KResult (here "Bool") *)
  let f : string -> string list -> string -> string = fun key v acc -> (* TODO if there are several subsorts ? *)
    if List.mem _SORT_KRESULT v && List.mem sort_input v
    then key ^ acc
    else "" ^ acc
  in
  let new_s = StrMap.fold f sign.subsort "" in
  let new_s = if new_s = "" then sort_input else new_s in

  (* STEP 3:  *)
  let new_pattern : p_term = create_inj_var new_s sort_input var_name in (* _INJ {new_s} {sort_input} (var_name) *)

  (* STEP 4: Update the injection (here, replace "_INJ {BExp} {s} (E1)" by "_INJ {Bool} {s} (E1)") *)

  (create_rule (subst_sort lhs new_s var_name) (subst rhs new_pattern var_name), Uncond, default_prio)::acc

(** ---------------------------- *)
(** To translate cooling rules   *) (* For now, its a heating rule... *)
(** ---------------------------- *)

(** [get_cond_data_in_cooling_rule cond] returns the main variable of a condition, with its type.
    For example, if cond = LblnotBool'Unds'{}(LblisKResult{}(kseq{}(inj{SortAExp{}, SortKItem{}}(VarHOLE:SortAExp{}),dotk{}()))),
    get_cond_data_in_cooling_rule cond returns (VarHOLE, SortAExp). *)
let get_cond_data_in_cooling_rule : p_term option -> string * string = fun cond ->
  match cond with
  | Some (* for example: LblnotBool'Unds'{}(LblisKResult{}(kseq{}(inj{SortAExp{}, SortKItem{}}(VarHOLE:SortAExp{}),dotk{}()))) *)
    ( {elt=P_Appl(
               {elt=P_Appl(
                        {elt=P_Iden({elt=(_, s_and);pos=_}, _);pos=_}, _) (* true /\ true *)
               ; pos=_},

               {elt=P_Appl(
                        {elt=P_Iden({elt=(_, s_not);pos=_}, _);pos=_},
                        {elt=P_Appl(
                                 {elt=P_Iden({elt=(_, s_kresult);pos=_}, _);pos=_},
                                 {elt=P_Appl(
                                          {elt=P_Appl({elt=P_Iden({elt=(_, s_kseq);pos=_}, _);pos=_},
                                                      {elt=P_Appl(
                                                               {elt=P_Appl(
                                                                        {elt=P_Appl({elt=P_Iden({elt=(_, s_inj);pos=_}, _);pos=_},
                                                                                    {elt=P_Expl({elt=P_Iden ({elt=(_,s1) ;pos=_}, _); pos=_}) ; pos=_} )
                                                                        ; pos=_},
                                                                        {elt=P_Expl({elt=P_Iden ({elt=(_,_) ;pos=_}, _); pos=_}) ; pos=_} )
                                                               ; pos=_},
                                                               {elt=P_Patt(Some {elt=n ;pos=_}, _) ; pos=_} )
                                                      ; pos=_} )
                                          ; pos=_},
                                          {elt=P_Iden({elt=(_, s_dotk);pos=_}, _);pos=_} )
                                 ; pos=_} )
                        ; pos=_} )
               ; pos=_} )

      ; pos=_}

    ) -> if s_and = pp _AND_BOOL && s_not = pp _NOT_BOOL
            && s_kresult = pp _IS_KRESULT && s_kseq = pp _KSEQ
            && s_inj = pp _INJ && s_dotk = pp _DOTK
         then n, s1 (* for example: VarHOLE, SortAExp *)
         else raise (NotYetImplemented "Unexpected shape for the condition.")
  | Some _ -> raise (NotYetImplemented "Unexpected shape for the condition.")
  | None   -> raise (InternalError "No condition for a cooling rule.")

(** [gen_new_pattern sym] generates the new pattern, as $\flat1 and $\flat2 *) (* TODO *)
let gen_new_pattern : symbol -> p_term = fun sym ->
  (* TODO Take into count "qv_l" in symbol = name * quant_var list * param list * param *)
  let name, _, p_l, _ = sym in
  let rec aux : int -> p_term -> p_term = fun i acc ->
    if i = 0 then acc
    else
      let new_name = Viry.safe_prefix ^ (string_of_int i) in (* create_pattern_var ("y" ^ (string_of_int i)) *)
      aux (i-1) ({elt=P_Appl(acc,{elt=P_Patt(Some {elt=new_name ; pos=None}, None); pos=None});pos=None})
  in
  aux (List.length p_l) (create_ident name)

let is_subsort_KResult : signature -> string -> bool = fun sign s ->
  let subsort_l =
    try
      StrMap.find s sign.subsort
    with Not_found -> raise (InternalError ("No sort " ^ s))
  in
  List.mem _SORT_KRESULT subsort_l

let trans_cooling_rule : attribute list -> ctrs_rule list -> signature -> alias -> quant_var list * axiom -> ctrs_rule list =
  fun attr_l acc sign al (_, ax) ->
  (* To understand this algorithm, consider the following example:
        rule E1 and E2 => E1 ~> (freezer1_and E2) requires not(E1 ∈ KResult) (règle C) *)

  (* STEP 0: Translate Kore pattern into Dedukti term *)
  let default_prio = 42 in
  let lhs, local_data, cond = create_LHS al sign in
  let rhs = create_RHS ax sign local_data in

  (* STEP 1: Get the variable to destruct with its type (here "E1" and "BExp") *)
  let new_v, sort_v = get_cond_data_in_cooling_rule cond in


  (* STEP 2: Generate a rule for each constructor of [sort_v] *)

  (* A. Get the constructors associated (here, "and", "<" and "not") *)
  let constructor_l =
    try
      Induc.find sort_v sign.inductive
    with Not_found -> raise (InternalError ("No constructor symbol for the sort " ^ sort_v))
  in

  (* B. Generate the new pattern, as ($X1 and $X2), ($X1 < $X2) and (not $X1) *)
  let lambda_lhs p = subst lhs p new_v in (* To replace each variable by the new pattern *)
  let lambda_rhs p = subst rhs p new_v in (* To replace each variable by the new pattern *)
  let gen_specialization : ctrs_rule list -> symbol -> ctrs_rule list = fun acc sym ->
    let new_pattern = gen_new_pattern sym in
    (create_rule (lambda_lhs new_pattern) (lambda_rhs new_pattern), Uncond, default_prio)::acc
  in
  (* C. Replace each variable by the new pattern, for each constructor *)
  let tmp_res = List.fold_left gen_specialization acc constructor_l in

  (* STEP 3: Generate a rule for each subsort of [sort_v] that isn't a subsort of KResult *)

  (* A. Get the subsort relations associated (can be "SortIden") *)
  let subsort_rel_l =
    let f key s_l l = if List.mem sort_v s_l && not(is_subsort_KResult sign key) then key::l else l in
    try
      StrMap.fold f sign.subsort []
    with Not_found -> raise (InternalError ("No constructor symbol for the sort " ^ sort_v))
  in
  (* B. Generate the new pattern, as _INJ {SortIden} {s2} E1 *)
  let curr_pattern s1 = create_inj_var s1 sort_v new_v in
  let gen_specialization : ctrs_rule list -> string -> ctrs_rule list = fun acc s ->
    (create_rule (subst lhs (curr_pattern s) new_v) (subst_sort rhs s new_v), Uncond, default_prio)::acc
  in
  (* C. Replace each variable by the new pattern, for each constructor *)
  List.fold_left gen_specialization tmp_res subsort_rel_l

(** ---------------------------- *)
(** To translate semantic rules  *)
(** ---------------------------- *)

let trans_semantic_rule : attribute list -> ctrs_rule list -> signature -> alias -> quant_var list * axiom -> ctrs_rule list =
  fun attr_l acc sign al (_, ax) ->
  (* Be careful: the order of the computation is important
     because of references *)
  let default_prio = 42 in
  let lhs, local_data, cond = create_LHS al sign in
  let rhs = create_RHS ax sign local_data in
  let attr_l =
    List.map (fun attr -> match attr with
                          | Owise _ -> true
                          | _ -> false) attr_l
  in
  let is_owise = List.fold_left (||) false attr_l in
  match is_owise, cond with
  | false, None   -> (create_rule lhs rhs, Uncond,     default_prio)::acc
  | false, Some x -> (create_rule lhs rhs, Cond x,     default_prio)::acc
  | true,  None   -> (create_rule lhs rhs, OwiseRule,  default_prio)::acc
  | true,  Some _ -> raise (InternalError "Case not possible in [trans_semantic_rule].")
