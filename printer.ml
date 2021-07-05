
open Type
open LP_printer
open Display_console
open Output

(** Lambdapi printer *)

let pp_import : Format.formatter -> count_data -> string list -> import -> unit =
  fun ppf cd path i ->
  incr_real_import cd ;
  pp_command ppf (Translate.import_to_require_open path i)

let pp_sort : Format.formatter -> count_data -> sort -> unit =
  fun ppf cd s -> incr_real_sort cd ;
                  pp_command ppf (Translate.sort_to_p_symbol (pp s))

let pp_induc : Format.formatter -> count_data -> sort * symbol list -> unit =
  fun ppf cd i -> incr_real_induc cd ; pp_command ppf (Translate.create_inductive_type i)

let pp_symbol : Format.formatter -> count_data -> symbol * attribute list -> unit =
  fun ppf cd ((name, qv_l, p_l, p), attr_l) ->
  let s = (pp name, qv_l, p_l, p) in
  incr_real_symbol cd ;
  pp_command ppf (Translate.symbol_to_p_symbol s attr_l)

let pp_alias : Format.formatter -> count_data ->
               alias * (quant_var list * axiom * attribute list) option -> unit =
  fun ppf cd v ->
  match v with
  | _, None -> () (* @TODO *)
  | al, Some(_,ax,_) ->
     try
       pp_command ppf (Translate.unconditional_rule_to_p_rule al ax) ;
       incr_real_rule cd
     with Axiom.ConditionalRule _ -> ()

let pp_alias_bis ppf al : unit = pp_command ppf (Symbol.alias_to_definition al)

let pp_axiom : Format.formatter -> count_data -> quant_var list * axiom * attribute list -> unit =
  fun ppf cd (_, ax, attr_l) ->
  match attr_l with
  | [Unit _] | [Assoc _] | [Idem _] ->
     (* if is_only_assoc ax then @TODO *)
     incr_real_rule cd ;
     pp_command ppf (Translate.equality_axiom_to_p_rule ax)
  | _ -> () (* @TODO *)

let pp_axiom_bis : Format.formatter -> count_data -> quant_var list * axiom -> unit =
  fun ppf _ (_,ax) ->
  match ax with
    | Rewrites(_,lhs,And(_,a1,a2)) ->
       if Axiom.is_conditional_rule a1 then
         raise (Axiom.ConditionalRule "Conditional rewriting rule not supported yet.")
       else
         pp_command ppf (LP_p_term.no_pos (Syntax.P_rules [LP_p_term.no_pos (Axiom.curry_pattern lhs, Axiom.curry_pattern a2)]))
    |  _ -> failwith "In RHS: Not yet implemented"

let pp_command : Format.formatter -> count_data -> command -> unit = fun ppf cd (c, attr_l) ->
  match c with
  | Sort     s -> incr_k_sort cd        ; pp_sort ppf cd s
  | H_sort   s -> incr_k_hooked_sort cd ; pp_sort ppf cd s
  | Symbol   s -> incr_k_symbol cd        ; pp_symbol ppf cd (s, attr_l)
  | H_symbol s -> incr_k_hooked_symbol cd ; pp_symbol ppf cd (s, attr_l)
  | Alias   al -> incr_k_alias cd ; pp_alias_bis ppf al (* @TODO : aller voir la suite de la liste *)
  | Axiom(qv_l, ax) -> incr_k_axiom cd ; pp_axiom ppf cd (qv_l, ax, attr_l)

let pp_command_bis  : Format.formatter -> count_data -> command list -> unit = fun ppf cd command_l ->
  let f_axiom :
        Format.formatter -> count_data -> attribute list -> unit -> quant_var list * axiom -> unit =
    fun ppf cd attr_l _ (qv_l, ax) ->
    match attr_l with
    | [] -> if Axiom.is_predicate_axiom ax then ()
            else pp_axiom ppf cd (qv_l, ax, attr_l)
    | _ -> pp_axiom ppf cd (qv_l, ax, attr_l)
  in
  kore_command_iter cd command_l ()
  (fun _ _ s -> pp_sort ppf cd s) (fun _ _ s -> pp_sort ppf cd s)
  (fun attr_l _ s -> pp_symbol ppf cd (s, attr_l)) (fun attr_l _ s -> pp_symbol ppf cd (s, attr_l))
  (fun _ _ _ -> ()) (fun attr_l _ ({lhs=al;rhs=(qv_l, ax)}) -> pp_alias ppf cd (al, Some (qv_l, ax, attr_l))) (f_axiom ppf cd)

let pp_command_ter : Format.formatter -> count_data -> command list -> unit  = fun ppf cd command_l ->
  let do_nothing : attribute list -> 'a -> quant_var list * axiom -> 'a = fun _ acc _ -> acc in
  let equality_axiom = fun attr_l _ (qv_l, ax) -> pp_axiom ppf cd (qv_l, ax, attr_l) in
   let f_axiom :
        Format.formatter -> count_data -> attribute list -> unit -> quant_var list * axiom -> unit =
    fun ppf cd attr_l _ (qv_l, ax) ->
    match attr_l with
    | [] -> if Axiom.is_predicate_axiom ax then ()
            else pp_axiom ppf cd (qv_l, ax, attr_l)
    | _ -> pp_axiom ppf cd (qv_l, ax, attr_l)
  in
  kore_command_iter_bis cd command_l ()
  (fun _ _ s -> pp_sort ppf cd s) (fun _ _ s -> pp_sort ppf cd s)
  (fun attr_l _ s -> pp_symbol ppf cd (s, attr_l)) (fun attr_l _ s -> pp_symbol ppf cd (s, attr_l))
  (fun _ _ al -> pp_alias_bis ppf al) (fun _ _ ax -> pp_axiom_bis ppf cd ax) (f_axiom ppf cd)
  do_nothing do_nothing do_nothing do_nothing do_nothing
  equality_axiom equality_axiom equality_axiom equality_axiom
  do_nothing (f_axiom ppf cd)

(** Kore printer *)

type output  = Format.formatter
let printing = Format.fprintf

let verbose = ref false

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

let pp_kore_symbol : output -> string -> symbol -> unit =
  fun ppf keyword (name, qv_l, p_l, p) ->
  let prints = printing ppf "%s" in
  prints keyword ; prints " " ;
  prints (pp name) ;
  pp_kore_quant_var_list ppf qv_l ;
  pp_kore_param_list ppf p_l ;
  printing ppf " : " ; pp_kore_param ppf p ; prints " "

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

let pp_kore_alias : output -> symbol -> (name * quant_var list * (name * param) list * def) -> unit =
  fun ppf sym (n, qv_l, p_l, def) ->
  pp_kore_symbol ppf "alias" sym ; pp_endline ppf ;
  printing ppf "where %s" (pp n) ;
  pp_kore_quant_var_list ppf qv_l ;
  let f ppf (n,p) = printing ppf "%s : " (pp n) ; pp_kore_param ppf p in
  pp_list ppf "(" f p_l ", " ") :=" ;
  pp_kore_def ppf def

let pp_kore_import : output -> count_data -> import -> unit = fun ppf cd (n, attr_l) ->
  incr_real_import cd;
  printing ppf "import %s " (pp n);
  pp_kore_attribute_list ppf attr_l

let pp_kore_command : output -> count_data -> command -> unit = fun ppf cd (c, attr_l) ->
  let pp_attr = fun () -> pp_kore_attribute_list ppf attr_l in
  (match c with
   | Sort     s -> incr_k_sort cd ; incr_real_sort cd ;
                   printing ppf "sort %s " (pp s) ; pp_attr()
   | H_sort   s -> incr_k_hooked_sort cd ; incr_real_sort cd ;
                   printing ppf "hooked-sort %s " (pp s) ; pp_attr()
   | Symbol   s -> incr_k_symbol cd ; incr_real_symbol cd ;
                   pp_kore_symbol ppf "symbol" s ; pp_attr()
   | H_symbol s -> incr_k_hooked_symbol cd ; incr_real_symbol cd ;
                   pp_kore_symbol ppf "hooked-symbol" s ; pp_attr()
   | Alias(sym, body) -> incr_k_alias cd ; pp_kore_alias ppf sym body ; pp_attr()
   | Axiom(qv_l, ax) -> incr_k_axiom cd ;
                        printing ppf "%s" "axiom" ; pp_kore_quant_var_list ppf qv_l ;
                        pp_endline ppf ; space ppf ; pp_kore_axiom ppf 2 ax ;
                        pp_endline ppf ; pp_kore_attribute_list ppf attr_l ; pp_endline ppf) ;
  pp_endline ppf
