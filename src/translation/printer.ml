
open Common.Type
open Common.Count_data
open Iterator   (* @TODO improve? *)
open! LP_interface.Syntax
open LP_interface.Output

type output  = Format.formatter
type printer = output -> p_command -> unit

(** Lambdapi printer *)

let pp_import : output -> count_data -> printer -> string list -> import -> unit =
  fun ppf cd prt path i ->
  incr_real_import cd ;
  prt ppf (Translate.import_to_require_open path i)

let pp_sort : output -> count_data -> printer -> sort -> unit =
  fun ppf cd prt s ->
  incr_real_sort cd ;
  prt ppf (Translate.sort_to_p_symbol (pp s))

let pp_induc : output -> count_data -> printer -> sort * symbol list -> unit =
  fun ppf cd prt i ->
  incr_real_induc cd ;
  prt ppf (Translate.create_inductive_type i)

let pp_symbol : output -> count_data -> printer -> symbol * attribute list -> unit =
  fun ppf cd prt ((name, qv_l, p_l, p), attr_l) ->
  let s = (pp name, qv_l, p_l, p) in
  incr_real_symbol cd ;
  prt ppf (Translate.symbol_to_p_symbol s attr_l)

let pp_alias : output -> count_data -> printer ->
               alias * (quant_var list * axiom * attribute list) option -> unit =
  fun ppf cd prt v ->
  match v with
  | _, None -> () (* @TODO *)
  | al, Some(_,ax,_) ->
     try
       prt ppf (Translate.unconditional_rule_to_p_rule al ax) ;
       incr_real_rule cd
     with Axiom.ConditionalRule _ -> ()

let pp_alias_bis ppf prt al : unit = prt ppf (Symbol.alias_to_definition al)

let pp_axiom : output -> count_data -> printer -> quant_var list * axiom * attribute list -> unit =
  fun ppf cd prt (_, ax, attr_l) ->
  match attr_l with
  | [Unit _] | [Assoc _] | [Idem _] ->
     (* if is_only_assoc ax then @TODO *)
     incr_real_rule cd ;
     prt ppf (Translate.equality_axiom_to_p_rule ax)
  | _ -> () (* @TODO *)

let pp_equality_axiom : output -> count_data -> printer -> quant_var list * axiom -> unit =
  fun ppf cd prt (_, ax) ->
  incr_real_rule cd ;
  prt ppf (Translate.equality_axiom_to_p_rule ax)

let pp_axiom_bis : output -> count_data -> printer -> quant_var list * axiom -> unit =
  fun ppf _ prt (_,ax) ->
  match ax with
    | Rewrites(_,lhs,And(_,a1,a2)) ->
       if Axiom.is_conditional_rule a1 then
         raise (Axiom.ConditionalRule "Conditional rewriting rule not supported yet.")
       else
         prt ppf (LP_interface.LP_p_term.no_pos (P_rules [LP_interface.LP_p_term.no_pos (Axiom.curry_pattern lhs, Axiom.curry_pattern a2)]))
    |  _ -> failwith "In RHS: Not yet implemented"

let pp_kommand : output -> count_data -> printer -> kommand -> unit =
  fun ppf cd prt (kommand, attr_l) ->
  match kommand with
  | Sort          s -> pp_sort ppf cd prt s
  | H_sort        s -> pp_sort ppf cd prt s
  | Symbol        s -> pp_symbol ppf cd prt (s, attr_l)
  | H_symbol      s -> pp_symbol ppf cd prt (s, attr_l)
  | Alias        al -> pp_alias_bis ppf prt al (* @TODO : aller voir la suite de la liste *)
  | Axiom(qv_l, ax) -> pp_axiom ppf cd prt (qv_l, ax, attr_l)

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

(** Kore printer *)

let verbose = ref false

let printing = Format.fprintf

let pp_endline ppf = printing ppf "\n"
let pp_paren ppf = printing ppf ")"
let space : output -> unit = fun ppf -> printing ppf "  "
let rec alignment : output -> int -> unit = fun ppf n ->
  if n <= 0 then () else (space ppf ; alignment ppf (n-1))

let pretty_string : (string * string) list -> string -> string = fun iso s ->
  let rec aux l s = match l with
     | [] -> s
     | (pattern, new_s)::t ->
        aux t (Str.global_replace (Str.regexp pattern) new_s s)
  in
  aux iso s

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


let pp_list : output -> string -> (output -> 'a -> unit) -> 'a list -> string -> string -> unit =
  fun ppf first f l separator last ->
  let prints = printing ppf "%s" in
  prints first ;
  let rec aux l = match l with
    | []  -> prints " "
    | [t] -> f ppf t
    | t1::t2::q -> f ppf t1 ; prints separator ; aux (t2::q)
  in
  aux l ; prints last

let pp_kore_param : output -> param -> unit = fun ppf p -> match p with
  | S s  -> printing ppf "%s" (pp s)
  | Q qv -> printing ppf "%s" (pp qv)

let pp_kore_quant_var_list : output -> quant_var list -> unit = fun ppf qv_l ->
  let f_qv ppf qv = printing ppf "%s" (pp qv) in
  pp_list ppf "{" f_qv qv_l "," "}"

let pp_kore_param_list : output -> param list -> unit = fun ppf p_l ->
  pp_list ppf "(" pp_kore_param p_l  "," ")"

let pp_kore_param_list_bis : output -> param list -> unit = fun ppf p_l ->
  pp_list ppf "{" pp_kore_param p_l  "," "}"

let pp_kore_attribute : output -> attribute -> unit = fun ppf attr ->
  let print = printing ppf "%s" in
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

let pp_kore_attribute_list : output -> attribute list -> unit = fun ppf attr_l ->
  pp_list ppf "[" pp_kore_attribute attr_l  ", " "]"

let pp_kore_sort : output -> sort -> attribute list -> unit =
  fun ppf s attr_l -> printing ppf "sort %s " (pp s) ; pp_kore_attribute_list ppf attr_l

let pp_kore_hooked_sort : output -> sort -> attribute list -> unit =
  fun ppf s attr_l -> printing ppf "hooked-sort %s " (pp s) ; pp_kore_attribute_list ppf attr_l

let pp_kore_symbol : output -> string -> symbol -> attribute list -> unit =
  fun ppf keyword (name, qv_l, p_l, p) attr_l ->
  let prints = printing ppf "%s" in
  prints keyword ; prints " " ;
  prints (pp name) ;
  pp_kore_quant_var_list ppf qv_l ;
  pp_kore_param_list ppf p_l ;
  printing ppf " : " ; pp_kore_param ppf p ; prints " " ;
  pp_kore_attribute_list ppf attr_l

let rec pp_kore_axiom : output -> int -> axiom -> unit = fun ppf step ax ->
  let print  = printing ppf in
  let prints = printing ppf "%s" in
  let tmp : param list -> int -> axiom -> axiom -> unit =
    fun p_l step ax1 ax2 ->
    pp_kore_param_list ppf p_l ;
    pp_endline ppf ;
    alignment ppf step ; pp_kore_axiom ppf (step+1) ax1 ;
    prints ",\n" ;
    alignment ppf step ; pp_kore_axiom ppf (step+1) ax2
  in
  let tmp2 : param list -> name -> param -> int -> axiom -> unit =
    fun p_l n p step ax ->
    pp_kore_param_list ppf p_l ;
    pp_endline ppf ;
    alignment ppf step ; print "%s : " (pp n) ; pp_kore_param ppf p ;
    print "%s" ",\n" ;
    alignment ppf step ; pp_kore_axiom ppf (step+1) ax
  in
  match ax with
  | Equals(p_l, ax1, ax2) ->
     prints "#EQUALS(" ; tmp p_l step ax1 ax2 ; pp_paren ppf
  | Exists(p_l, (n,p), ax) ->
     prints "#EXISTS(" ; tmp2 p_l n p step ax ; pp_paren ppf
  | And(p_l, ax1, ax2) ->
     prints "#AND(" ; tmp p_l step ax1 ax2 ; pp_paren ppf
  | Or(p_l, ax1, ax2) ->
     prints "#OR(" ; tmp p_l step ax1 ax2 ; pp_paren ppf
  | Not(p_l, ax) ->
     prints "#NOT(" ; pp_kore_param_list ppf p_l ;
     pp_endline ppf ; alignment ppf step ; pp_kore_axiom ppf (step+1) ax ; pp_paren ppf
  | Implies(p_l, ax1, ax2) ->
     prints "#IMPLIES(" ; tmp p_l step ax1 ax2 ; pp_paren ppf
  | Bottom p_l ->
     prints "#BOTTOM" ; pp_kore_param_list_bis ppf p_l
  | Top p_l ->
     prints "#TOP" ; pp_kore_param_list_bis ppf p_l
  | Rewrites(p_l, ax1, ax2) ->
     prints "#REWRITES(" ; tmp p_l step ax1 ax2 ; pp_paren ppf
  | In(p_l, (n,p), ax) ->
     prints "#IN(" ; tmp2 p_l n p step ax ; pp_paren ppf
  | Dom_val(sort, n) ->
     printing ppf "#DOMAIN_VALUES{%s}(%s)" (pp sort) (pp n)
  | Predicat p -> if !verbose then pp_kore_predicat_verbose ppf step p else pp_kore_predicat ppf step p
and pp_kore_predicat_verbose ppf step p = match p with
  | Sym(n, p_l, ax_l) ->
     printing ppf "#SYM(%s" (pp n) ; pp_kore_param_list_bis ppf p_l ;
     let f ppf ax = pp_endline ppf ; alignment ppf step ; pp_kore_axiom ppf (step+1) ax in
     pp_list ppf "(" f ax_l "," ")" ; pp_paren ppf
  | Var(n, p) -> printing ppf "#VAR(%s : " (pp n) ; pp_kore_param ppf p ; pp_paren ppf
and pp_kore_predicat ppf step p = match p with
  | Sym(n, p_l, ax_l) ->
     printing ppf "%s" (pp n) ; pp_kore_param_list_bis ppf p_l ;
     let f ppf ax = pp_endline ppf ; alignment ppf step ; pp_kore_axiom ppf (step+1) ax in
     pp_list ppf "(" f ax_l "," ")"
  | Var(n, p) -> printing ppf "%s : " (pp n) ; pp_kore_param ppf p

let pp_kore_def : output -> def -> unit = fun ppf def ->
  match def with
  | A ax     -> pp_endline ppf ; space ppf ; pp_kore_axiom ppf 2 ax
  | D(n, qv) -> printing ppf "%s : %s" (pp n) (pp qv)

let pp_kore_alias : output -> alias -> attribute list -> unit =
  fun ppf (sym, (n, qv_l, p_l, def)) attr_l ->
  pp_kore_symbol ppf "alias" sym attr_l ; pp_endline ppf ;
  printing ppf "where %s" (pp n) ;
  pp_kore_quant_var_list ppf qv_l ;
  let f ppf (n,p) = printing ppf "%s : " (pp n) ; pp_kore_param ppf p in
  pp_list ppf "(" f p_l ", " ") :=" ;
  pp_kore_def ppf def ;
  pp_kore_attribute_list ppf attr_l

let pp_kore_import : output -> import -> unit = fun ppf (n, attr_l) ->
  printing ppf "import %s " (pp n) ; pp_kore_attribute_list ppf attr_l

let pp_kore_kommand : output -> count_data -> kommand list -> unit = fun ppf cd kommand_l ->
  let f_sort attr_l _ s = pp_kore_sort ppf s attr_l in
  let f_symbol keyword attr_l _ sym = pp_kore_symbol ppf keyword sym attr_l in
  let f_axiom : attribute list -> unit -> quant_var list * axiom -> unit =
    fun attr_l _ (qv_l, ax) ->
    printing ppf "%s" "axiom" ; pp_kore_quant_var_list ppf qv_l ;
    pp_endline ppf ; space ppf ; pp_kore_axiom ppf 2 ax ;
    pp_endline ppf ; pp_kore_attribute_list ppf attr_l ; pp_endline ppf
  in
  kommand_iter_with_alias cd kommand_l ()
  f_sort f_sort (f_symbol "symbol") (f_symbol "hooked-symbol")
  (fun attr_l _ al -> pp_kore_alias ppf al attr_l)
  f_axiom f_axiom
  (f_axiom, f_axiom, f_axiom, f_axiom, f_axiom,
   f_axiom, f_axiom, f_axiom, f_axiom, f_axiom, f_axiom) (fun () -> pp_endline ppf)
