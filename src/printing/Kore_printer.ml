
open Common.Type
open Common.Error
open Mecanism.Iterator_plus_plus
open Interface.Output

type output = Format.formatter

let verbose = ref false

let pp_endline ppc = print ppc "\n"
let pp_paren   ppc = print ppc ")"
let space ppc : unit = print ppc "  "
let rec alignment ppc : int -> unit = fun n ->
  if n <= 0 then () else (space ppc ; alignment ppc (n-1))

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
  let f_axiom_bis :
        attribute list -> unit -> alias -> quant_var list * axiom -> unit =
    fun attr_l x _ (qv_l, ax) -> f_axiom attr_l x (qv_l, ax)
  in
  kommand_iter_with_alias cd kommand_l ()
  f_sort f_sort (f_symbol "symbol") (f_symbol "hooked-symbol")
  (fun attr_l _ al -> pp_kore_alias ppc al attr_l)
  (f_axiom_bis, f_axiom_bis, f_axiom_bis)
  f_axiom (f_axiom, f_axiom)
  (f_axiom, f_axiom, f_axiom, f_axiom) f_axiom f_axiom
  (f_axiom, f_axiom, f_axiom, f_axiom, f_axiom, f_axiom, f_axiom)
  (fun () -> pp_endline ppc)
