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

let curry_new : (string -> p_term) -> t -> signature -> p_term = fun f_var ax sign ->
  let rec aux : t -> p_term = fun ax ->
    let f_sym = fun (a:p_term) (b:t) : p_term -> create_appl a (aux b) in
    match ax with
    | Predicate p ->
       begin
         match p with
         | Sym(s, qv_l, a_l) when s = _INJ ->
            let g p = match p with S x | Q x -> create_implicit_arg x in
            let tmp = List.map g qv_l in
            let res = List.fold_left create_appl p_INJ tmp in
            List.fold_left f_sym res a_l
         | Sym(n, _, a_l) -> List.fold_left f_sym (create_ident n) a_l
         | Var(n, _) -> (if StrMap.mem n !data_matching
                         then
                           (let res = StrMap.find n !data_matching in
                            (if !do_specific_thing
                             then Axiom.change_sort_inj res sign
                             else res))
                         else f_var n)
       end
    | Equals(_, x, Dom_val(_, d)) when d = _TRUE  -> aux x
    | Equals(_, x, Dom_val(_, d)) when d = _FALSE -> create_appl (create_ident _NOT_BOOL) (aux x)
    | Equals _ -> raise (NotYetImplemented "Need to update [Eval_strategy_curry_new] - Case EQUALS")
    | Dom_val(_, name) -> create_ident name
    (*| In _ -> failwith "OK, guys" *)
    (*| Exists _ -> failwith "EXISTS"
      | Or _ -> failwith "OR"
      | Not _ -> failwith "NOT"
      | Implies _ -> failwith "IMPLIES"
      | Bottom _ -> failwith "BOTTOM"
      | Top    _ -> failwith "TOP"
      | Rewrites _ -> failwith "REWRITES" *)
    | And (_, ax1, Predicate(Var(n,_))) ->
       let res = aux ax1 in
       data_matching := StrMap.add n res !data_matching ; res
    | _ -> raise (NotYetImplemented "Need to update [Eval_strategy.curry_new].")
  in
  aux ax

let curry_condition = curry_new create_pattern_var

let create_LHS : alias -> signature -> p_term * p_term option = fun al sign ->
  let get_def : alias -> def = fun (_,(_,_,_,def)) -> def in
  let def = get_def al in
  match def with
  | A a ->
     begin
       match a with
       | And(_,a1,a2) ->
          (match a1 with
           | Top _ -> curry_pattern a2 sign, None
           | _     -> let res = curry_pattern a2 sign in res, Some (curry_condition a1 sign))
       |  _ -> raise (InternalError "The heating/cooling rule has no condition")
     end
  | D _ -> raise (NotYetImplemented "Alias (LHS) with a unique symbol as body.")

let create_RHS : t -> signature -> p_term = fun ax sign ->
  match ax with
  | Rewrites(_,_,And(_,a1,a2)) ->
     if is_conditional_rule a1 then
       raise (NotYetImplemented "KProver claim.")
     else
       Axiom.curry_pattern a2 sign
  |  _ -> raise (InternalError "The heating/cooling rule doesn't begin with \rewrites.")

(*        rule E1 and E2               => E1 ~> (freezer1_and E2) requires not(E1 ∈ KResult) (règle C)
       et rule E1 ~> (freezer1_and E2) => E1 and E2               requires E1 ∈ KResult      (règle H) *)

(** To translate cooling rules *)
let trans_cooling_rule : attribute list -> ctrs_rule list -> signature -> alias -> quant_var list * axiom -> ctrs_rule list =
  fun attr_l acc sign al (_, ax) ->
  do_specific_thing := true ;
  data_matching := StrMap.empty ; reset_var() ;
  (* Be careful: the order of the computation is important
     because of references *)
  let default_prio = 42 in
  let lhs, cond = create_LHS al sign in
  let rhs = create_RHS ax sign in
  do_specific_thing := false ;
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
  | true,  Some _ -> raise (InternalError "Case not possible in [trans_cooling_rule].")

(** To translate heating rules *)
let trans_heating_rule : attribute list -> ctrs_rule list -> signature -> alias -> quant_var list * axiom -> ctrs_rule list =
  fun attr_l acc sign al (_, ax) ->
  data_matching := StrMap.empty ; reset_var() ;
  (* Be careful: the order of the computation is important
     because of references *)
  let default_prio = 42 in
  let lhs, cond = create_LHS al sign in
  let rhs = create_RHS ax sign in
  do_specific_thing := false ;

  (* Selection of the variable to specialize, with its sort *)
  let new_v, sort_v = match cond with
    | Some (* Si de la forme : LblnotBool'Unds'{}(LblisKResult{}(kseq{}(inj{SortAExp{}, SortKItem{}}(VarHOLE:SortAExp{}),dotk{}())))) *)
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
           then n, s1
           else raise (NotYetImplemented "Unexpected shape for the condition.")
    | Some _ -> raise (NotYetImplemented "Unexpected shape for the condition.")
    | None   -> raise (InternalError "No condition for a heating rule.")
  in
  (* Get the list of constructor symbols *)
  let constr_sym_l =
    let natural_constr =
      try
        Induc.find sort_v sign.inductive
      with Not_found -> raise (InternalError ("No constructor symbol for the sort " ^ sort_v))
    in
    (* Get constructors by transitivty of subsort sorts *)
    let subsort_l =
      StrMap.fold (fun key s_l l -> if List.mem sort_v s_l then key::l else l) sign.subsort []
    in
    let f l s =
      let tmp = try Induc.find s sign.inductive with Not_found -> [] in
      tmp@l
    in
    List.fold_left f natural_constr subsort_l
  in
  (* Generation of specialize rules *)
  let subst t a =
    let rec aux : p_term -> p_term = fun t -> match t with
                                              | ({elt=P_Appl(t1, t2);pos=p}) ->
                                                 {elt=P_Appl(aux t1, aux t2);pos=p}
                                              | ({elt=P_Patt(Some {elt=x;pos=_}, _);pos=_}) ->
                                                 if x = new_v
                                                 then a
                                                 else t
                                              | t -> t
    in
    aux t
  in
  let lambda_lhs p = subst lhs p in
  let lambda_rhs p = subst rhs p in
  let gen_specialization : ctrs_rule list -> symbol -> ctrs_rule list = fun acc sym ->
    (* Generate the new pattern *)
    (* TODO Take into count "qv_l" in symbol = name * quant_var list * param list * param *)
    let name, _, p_l, _ = sym in
    let new_pattern =
      let rec aux : int -> p_term -> p_term = fun i acc ->
        if i = 0 then acc
        else
          let new_name = Viry.safe_prefix ^ (string_of_int i) in
          aux (i-1) ({elt=P_Appl(acc,{elt=P_Patt(Some {elt=new_name ; pos=None}, None); pos=None});pos=None})
      in
      aux (List.length p_l) (create_ident name)
    in
    (* Use it *)
    (create_rule (lambda_lhs new_pattern) (lambda_rhs new_pattern), Uncond, default_prio)::acc
  in
  List.fold_left gen_specialization acc constr_sym_l


(** To translate semantic rules *)
let trans_semantic_rule : attribute list -> ctrs_rule list -> signature -> alias -> quant_var list * axiom -> ctrs_rule list =
  fun attr_l acc sign al (_, ax) ->
  data_matching := StrMap.empty ; reset_var() ;
  (* Be careful: the order of the computation is important
     because of references *)
  let default_prio = 42 in
  let lhs, cond = create_LHS al sign in
  let rhs = create_RHS ax sign in
  do_specific_thing := false ;
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
