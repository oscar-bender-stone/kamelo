
open Common.Type
open Mecanism.Count_data
open Mecanism.Iterator_plus_plus   (* @TODO improve? *)
open! LP.Syntax
open Interface.Output

open Translation

open Common.Error

type output  = Format.formatter
type printer = output -> p_command -> unit

(** Lambdapi printer *)

let pp_import ppc cd prt : string list -> import -> unit = fun path i ->
  incr_real_import cd ;
  prt ppc (Translate.import_to_require_open path i)

let pp_sort ppc cd prt : sort -> unit = fun s ->
  (* incr_real_sort cd ; *)
  incr_real_symbol cd ;
  prt ppc (Translate.sort_to_p_symbol (pp s))

let pp_induc ppc cd prt : sort * symbol list -> unit = fun i ->
  incr_real_induc cd ;
  prt ppc (Translate.create_inductive_type i)

let pp_symbol ppc cd prt : symbol * attribute list -> unit =
  fun ((name, qv_l, p_l, p), attr_l) ->
  let s = (pp name, qv_l, p_l, p) in
  incr_real_symbol cd ;
  prt ppc (Translate.symbol_to_p_symbol s attr_l)

let pp_alias ppc cd prt :
      alias * (quant_var list * axiom * attribute list) option -> unit =
  fun v ->
  match v with
  | _, None -> () (* @TODO *)
  | al, Some(_,ax,_) ->
     try
       prt ppc (Translate.unconditional_rule_to_p_rule al ax) ;
       incr_real_rule cd
     with Axiom.ConditionalRule _ -> ()

let pp_alias_bis ppc prt al : unit = prt ppc (Alias.alias_to_definition al)

let pp_axiom ppc cd prt : quant_var list * axiom * attribute list -> unit =
  fun (_, ax, attr_l) ->
  match attr_l with
  | [Unit _] | [Assoc _] | [Idem _] ->
     (* if is_only_assoc ax then @TODO *)
     incr_real_rule cd ;
     prt ppc (Translate.equality_axiom_to_p_rule ax)
  | _ -> () (* @TODO *)

let pp_equality_axiom ppc cd prt : quant_var list * axiom -> unit =
  fun (_, ax) ->
  incr_real_rule cd ;
  prt ppc (Translate.equality_axiom_to_p_rule ax)

let pp_axiom_bis ppc _ (* TODO fix *) prt : quant_var list * axiom -> unit = fun (_,ax) ->
  match ax with
    | Rewrites(_,lhs,And(_,a1,a2)) ->
       if is_conditional_rule a1 then
         raise (Axiom.ConditionalRule "Conditional rewriting rule not supported yet.")
       else
         prt ppc (Interface.LP_p_term.no_pos (P_rules [Interface.LP_p_term.no_pos (Axiom.curry_pattern lhs, Axiom.curry_pattern a2)]))
    |  _ -> failwith "In RHS: Not yet implemented"

let pp_kommand ppc cd prt : kommand -> unit = fun (kommand, attr_l) ->
  match kommand with
  | Sort          s -> pp_sort ppc cd prt s
  | H_sort        s -> pp_sort ppc cd prt s
  | Symbol        s -> pp_symbol ppc cd prt (s, attr_l)
  | H_symbol      s -> pp_symbol ppc cd prt (s, attr_l)
  | Alias        al -> pp_alias_bis ppc prt al (* @TODO : aller voir la suite de la liste *)
  | Axiom(qv_l, ax) -> pp_axiom ppc cd prt (qv_l, ax, attr_l)

(*

let pp_kommand_bis  : output -> count_data -> printer -> kommand list -> unit = fun ppf cd prt kommand_l ->
  let do_nothing = fun _ _ _ -> () in
  let equality_axiom = fun _ _ (qv_l, ax) -> pp_equality_axiom ppf cd prt (qv_l, ax) in
  let f_axiom : attribute list -> unit -> quant_var list * axiom -> unit =
    fun attr_l _ (qv_l, ax) ->
    match attr_l with
    | [] -> if Axiom.is_predicate ax then ()
            else pp_axiom ppf cd prt (qv_l, ax, attr_l)
    | _ -> pp_axiom ppf cd prt (qv_l, ax, attr_l)
  in
  kommand_iter_without_alias cd kommand_l ()
  (fun _ _ s -> pp_sort ppf cd prt s) (fun _ _ s -> pp_sort ppf cd prt s)
  (fun attr_l _ s -> pp_symbol ppf cd prt (s, attr_l)) (fun attr_l _ s -> pp_symbol ppf cd prt (s, attr_l))
  do_nothing (fun attr_l _ ({lhs=al;rhs=(qv_l, ax)}) -> pp_alias ppf cd prt (al, Some (qv_l, ax, attr_l)))
  f_axiom
  (do_nothing, do_nothing, do_nothing, do_nothing, do_nothing,
   equality_axiom, equality_axiom, equality_axiom, equality_axiom,
   do_nothing, do_nothing) (fun () -> ())

let pp_kommand_ter : output -> count_data -> printer -> kommand list -> unit  = fun ppf cd prt kommand_l ->
  let do_nothing : attribute list -> 'a -> quant_var list * axiom -> 'a = fun _ acc _ -> acc in
  let equality_axiom = fun _ _ (qv_l, ax) -> pp_equality_axiom ppf cd prt (qv_l, ax) in
   let f_axiom : attribute list -> unit -> quant_var list * axiom -> unit =
    fun attr_l _ (qv_l, ax) ->
    match attr_l with
    | [] -> if Axiom.is_predicate ax then ()
            else pp_axiom ppf cd prt (qv_l, ax, attr_l)
    | _ -> pp_axiom ppf cd prt (qv_l, ax, attr_l)
  in
  kommand_iter_with_alias cd kommand_l ()
  (fun _ _ s -> pp_sort ppf cd prt s) (fun _ _ s -> pp_sort ppf cd prt s)
  (fun attr_l _ s -> pp_symbol ppf cd prt (s, attr_l)) (fun attr_l _ s -> pp_symbol ppf cd prt (s, attr_l))
  (fun _ _ al -> pp_alias_bis ppf prt al) (fun _ _ ax -> pp_axiom_bis ppf cd prt ax) f_axiom
  (do_nothing, do_nothing, do_nothing, do_nothing, do_nothing,
   equality_axiom, equality_axiom, equality_axiom, equality_axiom,
   do_nothing, (fun attr_l -> f_axiom attr_l)) (fun () -> ())

  *)

open Translation.Axiom
open Interface.LP_p_term
open Interface.K_prelude

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




















(** Kore printer *)

let verbose = ref false

let pp_endline ppc = print ppc "\n"
let pp_paren   ppc = print ppc ")"
let space ppc : unit = print ppc "  "
let rec alignment ppc : int -> unit = fun n ->
  if n <= 0 then () else (space ppc ; alignment ppc (n-1))

let pretty_string : (string * string) list -> string -> string = fun iso s ->
  let rec aux l s = match l with
     | [] -> s
     | (pattern, new_s)::t ->
        aux t (Str.global_replace (Str.regexp pattern) new_s s)
  in
  aux iso s
(*
let skip_sign s = "_\\([A-Z-]*\\)" ^ s ^ "\\([A-Za-z_-]+\\)"

let string_symbol_isomorphism =
  [ ("Lbl", "") ; ("Var", "") ; ("Sort", "") ; ("Stop", ".") ; ("Unds", "_") ; ("'", "") ;  ("-LT-", "<") ; ("-GT-", ">")
  ; ("Pipe", "|") ; ("Eqls", "=") ; ("Slsh", "/") ; ("Hash", "#") ; ("Tild", "~") ; ("Perc", "%") ; ("Star", "*") ; ("Quot", "'")
  ; ("projectColn", "proj_") (*; ("project", "π") *) ; ("Plus", "+")
  ; ("LPar", "(") ; ("RPar", ")") ; ("LSqB", "[") ; ("RSqB", "]") ; ("LBra", "{") ; ("RBra", "}")
  ; ("Comm", ",") ; ("Coln", ":") ; ("SCln", ";") ; ("LPar_\\([Comm_]*\\)RPar", "")
  ; (skip_sign "-SYNTAX", "_") ; (skip_sign "-COMMON", "")
  ; (skip_sign "INT", "_INT") ; (skip_sign "LIST", "_LIST") ; (skip_sign "SET", "_SET") ; (skip_sign "MAP", "_MAP") ]

let pp s = if !readable then pretty_string string_symbol_isomorphism s else s
 *)

let pp_list ppc : string -> (output -> 'a -> unit) -> 'a list -> string -> string -> unit =
  fun first f l separator last ->
  let prints = print ppc "%s" in
  prints first ;
  let rec aux l = match l with
    | []  -> prints " "
    | [t] -> f ppc t
    | t1::t2::q -> f ppc t1 ; prints separator ; aux (t2::q)
  in
  aux l ; prints last

let pp_kore_param ppc : param -> unit = fun p -> match p with
  | S s  -> print ppc "%s" (pp s)
  | Q qv -> print ppc "%s" (pp qv)

let pp_kore_quant_var_list ppc : quant_var list -> unit = fun qv_l ->
  let f_qv ppc qv = print ppc "%s" (pp qv) in
  pp_list ppc "{" f_qv qv_l "," "}"

let pp_kore_param_list ppc : param list -> unit = fun p_l ->
  pp_list ppc "(" pp_kore_param p_l  "," ")"

let pp_kore_param_list_bis ppc : param list -> unit = fun p_l ->
  pp_list ppc "{" pp_kore_param p_l  "," "}"

let pp_kore_attribute ppc : attribute -> unit = fun attr ->
  let print = print ppc "%s" in
  match attr with
  | Assoc       _ -> print "ASSOC"
  | Comm        _ -> print "COMM"
  | Idem        _ -> print "IDEM"
  | Unit        _ -> print "UNIT"

  | Strict      _ -> print "STRICT"
  | Seqstrict   _ -> print "SEQSTRICT"

  | Cool        _ -> print "COOL"
  | CoolLike    _ -> print "COOL-LIKE"
  | Heat        _ -> print "HEAT"
  | Structural  _ -> print "STRUCTURAL"

  | Simpl       _ -> print "SIMPLIFICATION"

  | Left        _ -> print "LEFT"
  | Right       _ -> print "RIGHT"
  | Priorities  _ -> print "PRIORITIES"

  | Constructor _ -> print "CONSTRUCTOR"
  | Injective   _ -> print "INJECTIVE"
  | Predicate   _ -> print "PREDICATE"

  | Functional  _ -> print "FUNCTIONAL"
  | Function    _ -> print "FUNCTION"

  | Anywhere    _ -> print "ANYWHERE"
  | Owise       _ -> print "OWISE"

  | Subsort     _ -> print "SUBSORT"
  | Projection  _ -> print "PROJECTION"
  | Initializer _ -> print "INITIALIZER"

  | Other(s, _)   -> print s

let pp_kore_attribute_list ppc : attribute list -> unit = fun attr_l ->
  pp_list ppc "[" pp_kore_attribute attr_l  ", " "]"

let pp_kore_sort ppc : sort -> attribute list -> unit = fun s attr_l ->
  print ppc "sort %s " (pp s) ; pp_kore_attribute_list ppc attr_l

let pp_kore_hooked_sort ppc : sort -> attribute list -> unit =
  fun s attr_l -> print ppc "hooked-sort %s " (pp s) ; pp_kore_attribute_list ppc attr_l

let pp_kore_symbol ppc : string -> symbol -> attribute list -> unit =
  fun keyword (name, qv_l, p_l, p) attr_l ->
  let prints = print ppc "%s" in
  prints keyword ; prints " " ;
  prints (pp name) ;
  pp_kore_quant_var_list ppc qv_l ;
  pp_kore_param_list ppc p_l ;
  print ppc " : " ; pp_kore_param ppc p ; prints " " ;
  pp_kore_attribute_list ppc attr_l

let rec pp_kore_axiom ppc : int -> axiom -> unit = fun step ax ->
  let prints = print ppc "%s" in
  let tmp : param list -> int -> axiom -> axiom -> unit =
    fun p_l step ax1 ax2 ->
    pp_kore_param_list ppc p_l ;
    pp_endline ppc ;
    alignment ppc step ; pp_kore_axiom ppc (step+1) ax1 ;
    prints ",\n" ;
    alignment ppc step ; pp_kore_axiom ppc (step+1) ax2
  in
  let tmp2 : param list -> name -> param -> int -> axiom -> unit =
    fun p_l n p step ax ->
    pp_kore_param_list ppc p_l ;
    pp_endline ppc ;
    alignment ppc step ; print ppc "%s : " (pp n) ; pp_kore_param ppc p ;
    print ppc "%s" ",\n" ;
    alignment ppc step ; pp_kore_axiom ppc (step+1) ax
  in
  match ax with
  | Equals(p_l, ax1, ax2) ->
     prints "#EQUALS(" ; tmp p_l step ax1 ax2 ; pp_paren ppc
  | Exists(p_l, (n,p), ax) ->
     prints "#EXISTS(" ; tmp2 p_l n p step ax ; pp_paren ppc
  | And(p_l, ax1, ax2) ->
     prints "#AND(" ; tmp p_l step ax1 ax2 ; pp_paren ppc
  | Or(p_l, ax1, ax2) ->
     prints "#OR(" ; tmp p_l step ax1 ax2 ; pp_paren ppc
  | Not(p_l, ax) ->
     prints "#NOT(" ; pp_kore_param_list ppc p_l ;
     pp_endline ppc ; alignment ppc step ;
     pp_kore_axiom ppc (step+1) ax ; pp_paren ppc
  | Implies(p_l, ax1, ax2) ->
     prints "#IMPLIES(" ; tmp p_l step ax1 ax2 ; pp_paren ppc
  | Bottom p_l ->
     prints "#BOTTOM" ; pp_kore_param_list_bis ppc p_l
  | Top p_l ->
     prints "#TOP" ; pp_kore_param_list_bis ppc p_l
  | Rewrites(p_l, ax1, ax2) ->
     prints "#REWRITES(" ; tmp p_l step ax1 ax2 ; pp_paren ppc
  | In(p_l, (n,p), ax) ->
     prints "#IN(" ; tmp2 p_l n p step ax ; pp_paren ppc
  | Dom_val(sort, n) ->
     print ppc "#DOMAIN_VALUES{%s}(%s)" (pp sort) (pp n)
  | Predicate p -> if !verbose then pp_kore_predicat_verbose ppc step p
                   else pp_kore_predicat ppc step p
and pp_kore_predicat_verbose ppc step p = match p with
  | Sym(n, p_l, ax_l) ->
     print ppc "#SYM(%s" (pp n) ; pp_kore_param_list_bis ppc p_l ;
     let f ppc ax =
       pp_endline ppc ; alignment ppc step ; pp_kore_axiom ppc (step+1) ax
     in
     pp_list ppc "(" f ax_l "," ")" ; pp_paren ppc
  | Var(n, p) ->
     print ppc "#VAR(%s : " (pp n) ; pp_kore_param ppc p ; pp_paren ppc
and pp_kore_predicat ppc step p = match p with
  | Sym(n, p_l, ax_l) ->
     print ppc "%s" (pp n) ; pp_kore_param_list_bis ppc p_l ;
     let f ppc ax =
       pp_endline ppc ; alignment ppc step ; pp_kore_axiom ppc (step+1) ax
     in
     pp_list ppc "(" f ax_l "," ")"
  | Var(n, p) -> print ppc "%s : " (pp n) ; pp_kore_param ppc p

let pp_kore_def ppc : def -> unit = fun def ->
  match def with
  | A ax     -> pp_endline ppc ; space ppc ; pp_kore_axiom ppc 2 ax
  | D(n, qv) -> print ppc "%s : %s" (pp n) (pp qv)

let pp_kore_alias ppc : alias -> attribute list -> unit =
  fun (sym, (n, qv_l, p_l, def)) attr_l ->
  pp_kore_symbol ppc "alias" sym attr_l ; pp_endline ppc ;
  print ppc "where %s" (pp n) ;
  pp_kore_quant_var_list ppc qv_l ;
  let f ppc (n,p) = print ppc "%s : " (pp n) ; pp_kore_param ppc p in
  pp_list ppc "(" f p_l ", " ") :=" ;
  pp_kore_def ppc def ;
  pp_kore_attribute_list ppc attr_l

let pp_kore_import ppc : import -> unit = fun (n, attr_l) ->
  print ppc "import %s " (pp n) ; pp_kore_attribute_list ppc attr_l

let pp_kore_kommand ppc cd : kommand list -> unit = fun kommand_l ->
  let f_sort attr_l _ s = pp_kore_sort ppc s attr_l in
  let f_symbol keyword attr_l _ sym =
    pp_kore_symbol ppc keyword sym attr_l
  in
  let f_axiom : attribute list -> unit -> quant_var list * axiom -> unit =
    fun attr_l _ (qv_l, ax) ->
    print ppc "%s" "axiom" ; pp_kore_quant_var_list ppc qv_l ;
    pp_endline ppc ; space ppc ; pp_kore_axiom ppc 2 ax ;
    pp_endline ppc ; pp_kore_attribute_list ppc attr_l ; pp_endline ppc
  in
  kommand_iter_with_alias cd kommand_l ()
  f_sort f_sort (f_symbol "symbol") (f_symbol "hooked-symbol")
  (fun attr_l _ al -> pp_kore_alias ppc al attr_l)
  f_axiom f_axiom (f_axiom, f_axiom)
  (f_axiom, f_axiom, f_axiom, f_axiom) f_axiom f_axiom
  (f_axiom, f_axiom, f_axiom, f_axiom,
   f_axiom, f_axiom, f_axiom) (fun () -> pp_endline ppc)
