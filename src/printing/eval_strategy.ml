open Translation.Axiom
open Interface.LP_p_term
open Interface.K_prelude

open Common.Type
open Common.Getter
open Common.Error
open LP.Syntax
open Printer

open Mecanism.Iterator_plus_plus
open Mecanism.Count_data

module Sort = struct
  type t = sort
  let compare = String.compare
end
module Induc = Map.Make(Sort)

let data_induc : (symbol list) Induc.t ref = ref Induc.empty

let encoding ppc cd prt : kommand list -> unit = fun kommand_l ->
  (* STEP 1: From K commands to CTRS rules (and partial printing). *)
  let f_sort _ acc s = pp_sort ppc cd prt s ; acc in
  let f_symbol attr_l acc s =
    (match is_constructor s attr_l with
     | Some sort ->
        let f new_v old_v = match old_v with None -> Some [new_v] | Some q -> Some (new_v::q) in
        data_induc := Induc.update sort (f s) !data_induc
     | None -> () ) ;
    pp_symbol ppc cd prt (s, attr_l) ; acc in
  let f_deleted  _ _ _ = [] in
  let propagation = fun _ x _ -> x in

  (*  axiom{R} \exists{R} (Val:SortKItem{}, \equals{SortKItem{}, R} (Val:SortKItem{}, inj{SortCell{}, SortKItem{}} (From:SortCell{}))) *)
  let collect_subsort_data :
      attribute list -> ctrs_rule list -> quant_var list * axiom ->
      ctrs_rule list = fun _ _ (_, ax) ->
    match ax with
    | Exists (_, _, Equals(_, _, Predicate(Sym("inj", [S s1; S s2], _)))) ->
       from_subsort_axiom s1 s2 ; []
    | _ -> failwith "Error in [Printer.collect_subsort_data]"
  in
  let curry_new : (string -> p_term) -> t -> p_term = fun f_var ax ->
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
           | Var(n, _) -> (if StrMap.mem n !data_matching
                           then
                             (let res = StrMap.find n !data_matching in
                              (if !do_specific_thing
                               then Translation.Axiom.change_sort_inj res
                               else res))
                           else f_var n)
         end
      | Equals(_, x, Dom_val(_, "true"))  -> aux x
      | Equals(_, x, Dom_val(_, "false")) -> create_appl (create_ident _NOT_BOOL) (aux x)
      | Equals _ -> failwith "EQUALS"
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
      | _ -> failwith "Not yet implemented [Axiom.curry]."
    in
    aux ax
  in
  let curry_condition = curry_new create_pattern_var in
  let create_LHS : alias -> p_term * p_term option = fun al ->
    let get_def : alias -> def = fun (_,(_,_,_,def)) -> def in
    let def = get_def al in
    match def with
    | A a ->
       begin
         match a with
         | And(_,a1,a2) ->
            (match a1 with
             | Top _ -> curry_pattern a2, None
             | _     -> let res = curry_pattern a2 in res, Some (curry_condition a1)) (* (no_pos P_Type)) *)
         |  _ -> failwith "In LHS: Not yet implemented"
       end
    | D _ -> failwith "Not possible in rewriting axiom"
  in
  let create_RHS : t -> p_term = fun ax ->
    match ax with
    | Rewrites(_,_,And(_,a1,a2)) ->
       if is_conditional_rule a1 then (print _STDOUT "One KProver claim." ; p_TYPE)
         (* raise (ConditionalRule "KProver claim not supported yet.") *)
       else
         Translation.Axiom.curry_pattern a2
    |  _ -> failwith "In RHS: Not yet implemented"
  in
  let create_ctrs_rule :
      attribute list -> alias -> axiom -> ctrs_rule list
    = fun attr_l al ax ->
    if is_cooling_rule attr_l then do_specific_thing := true ;
    (* wrn_1 _STDOUT "\n In CTRS VIRY %b" (is_cooling_rule attr_l); *)
    data_matching := StrMap.empty ; reset_var() ;
    (* Be careful: the order of the computation is important
       because of references *)
    let default_prio = 42 in
    let lhs, cond = create_LHS al in
    let rhs = create_RHS ax in
    do_specific_thing := false ;

    if is_heating_rule attr_l then
      (* Selection of the variable to specialize, with its sort *)
      let new_v, sort_v = match cond with
        | Some (* Si de la forme : LblnotBool'Unds'{}(LblisKResult{}(kseq{}(inj{SortAExp{}, SortKItem{}}(VarHOLE:SortAExp{}),dotk{}())))) *)
          ( {elt=P_Appl(
                 {elt=P_Appl(
                      {elt=P_Iden({elt=(_, "Lbl'Unds'andBool'Unds'");pos=_}, _);pos=_},
                      _) (* true /\ true *)
                 ; pos=_},



                 {elt=P_Appl(
                      {elt=P_Iden({elt=(_, "LblnotBool'Unds'");pos=_}, _);pos=_},
                      {elt=P_Appl(
                           {elt=P_Iden({elt=(_, "LblisKResult");pos=_}, _);pos=_},
                           {elt=P_Appl(
                                {elt=P_Appl({elt=P_Iden({elt=(_,"kseq");pos=_}, _);pos=_},
                                            {elt=P_Appl(
                                                 {elt=P_Appl(
                                                      {elt=P_Appl({elt=P_Iden({elt=(_,"inj");pos=_}, _);pos=_},
                                                                  {elt=P_Expl({elt=P_Iden ({elt=(_,s1) ;pos=_}, _); pos=_}) ; pos=_} )
                                                      ; pos=_},
                                                      {elt=P_Expl({elt=P_Iden ({elt=(_,_) ;pos=_}, _); pos=_}) ; pos=_} )
                                                 ; pos=_},
                                                 {elt=P_Patt(Some {elt=n ;pos=_}, _) ; pos=_} )
                                            ; pos=_} )
                                ; pos=_},
                                {elt=P_Iden({elt=(_,"dotk");pos=_}, _);pos=_} )
                           ; pos=_} )
                      ; pos=_} )
                 ; pos=_} )


            ; pos=_}




          ) -> n, s1

        | Some (* Si de la forme : LblnotBool'Unds'{}(LblisKResult{}(kseq{}(inj{SortAExp{}, SortKItem{}}(VarHOLE:SortAExp{}),dotk{}())))) *)
           ( {elt=P_Appl(
                  {elt=P_Appl(
                       {elt=P_Iden({elt=(_,"_andBool_");pos=_}, _);pos=_},
                       _) (* true /\ true *)
                  ; pos=_},



                  {elt=P_Appl(
                       {elt=P_Iden({elt=(_, "notBool_");pos=_}, _);pos=_},
                       {elt=P_Appl(
                            {elt=P_Iden({elt=(_,"isKResult");pos=_}, _);pos=_},
                            {elt=P_Appl(
                                 {elt=P_Appl({elt=P_Iden({elt=(_,"kseq");pos=_}, _);pos=_},
                                             {elt=P_Appl(
                                                  {elt=P_Appl(
                                                       {elt=P_Appl({elt=P_Iden({elt=(_,"inj");pos=_}, _);pos=_},
                                                                   {elt=P_Expl({elt=P_Iden ({elt=(_,s1) ;pos=_}, _); pos=_}) ; pos=_} )
                                                       ; pos=_},
                                                       {elt=P_Expl({elt=P_Iden ({elt=(_,_) ;pos=_}, _); pos=_}) ; pos=_} )
                                                  ; pos=_},
                                                  {elt=P_Patt(Some {elt=n;pos=_}, _) ; pos=_} )
                                             ; pos=_} )
                                 ; pos=_},
                                 {elt=P_Iden({elt=(_,"dotk");pos=_}, _);pos=_} )
                            ; pos=_} )
                       ; pos=_} )
                  ; pos=_} )


             ; pos=_}

           ) -> n, s1
        | Some _ -> failwith "Unexpected shape for the condition."
        | None -> failwith "No condition for a heating rule."
      in
      (* Get the list of constructor symbols *)
      let constr_sym_l =
        let natural_constr =
          try
            Induc.find sort_v !data_induc
          with Not_found -> failwith ("No constructor symbol for the sort" ^ sort_v)
        in
        (* Get constructors by transitivty of subsort sorts *)
        let subsort_l =
          StrMap.fold (fun key s_l l -> if List.mem sort_v s_l then key::l else l) !(Translation.Axiom.subsort_data) []
        in
        let f l s =
          let tmp = try Induc.find s !data_induc with Not_found -> [] in
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
              let new_name = Translation.Viry.safe_prefix ^ (string_of_int i) in
              aux (i-1) ({elt=P_Appl(acc,{elt=P_Patt(Some {elt=new_name ; pos=None}, None); pos=None});pos=None})
          in
          aux (List.length p_l) (create_ident name)
        in
        (* Use it *)
        (no_pos (lambda_lhs new_pattern, lambda_rhs new_pattern), Uncond, default_prio)::acc
      in
      List.fold_left gen_specialization [] constr_sym_l

    else
      let attr_l =
        List.map (fun attr -> match attr with
                              | Owise _ -> true
                              | _ -> false) attr_l
      in
      let is_owise = List.fold_left (||) false attr_l in
      match is_owise, cond with
      | false, None   -> [(no_pos (lhs, rhs), Uncond,     default_prio)]
      | false, Some x -> [(no_pos (lhs, rhs), Cond x,     default_prio)]
      | true,  None   -> [(no_pos (lhs, rhs), OwiseRule,  default_prio)]
      | true,  Some _ -> failwith "Not possible [create_ctrs_rule]."
  in
  let trans_implies =
    fun _ acc (_, ax) -> (of_implies_axiom ax)::acc
  in
  let ctrs_r_l =
    kommand_iter_without_alias cd kommand_l []
    f_sort f_deleted f_symbol f_deleted propagation
    (fun attr_l acc {lhs=al; rhs=(_, ax)} ->
      (create_ctrs_rule attr_l al ax)@acc)
    propagation (collect_subsort_data, propagation)
    (propagation, propagation, propagation, propagation)
    propagation propagation
    (propagation, trans_implies, trans_implies, trans_implies,
     propagation, trans_implies, trans_implies) (fun () -> ())
  in
  (* STEP 2: From CTRS rules to TRS rules and symbols. *)
  let sym_l, r_l = Translation.Viry.viry_encoding ctrs_r_l in
  (* STEP 3: Print symbols then TRS rules. *)
  if List.length sym_l > 3 then
    (List.iter
       (fun x -> incr_additional_symbol cd ; prt ppc (no_pos (P_symbol x)))
       (List.rev sym_l) ;
     List.iter
       (fun x -> incr_real_rule cd ; prt ppc (no_pos (P_rules  [x])))
       (List.rev r_l))
