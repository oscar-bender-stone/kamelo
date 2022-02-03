
open Error

(** Type to abstract the Kore files *)

type name = string
type sort = name
type quant_var = name
type param = S of sort | Q of quant_var

type data_attr = param list * string list

type attribute =
 (* USEFUL attributes *)
 | Assoc        of data_attr
 | Comm         of data_attr
 | Idem         of data_attr
 | Unit         of data_attr

 | Strict       of data_attr
 | Seqstrict    of data_attr

 | Cool         of data_attr
 | CoolLike     of data_attr
 | Heat         of data_attr
 | Structural   of data_attr

 | Simpl        of data_attr

 | Left         of data_attr
 | Right        of data_attr
 | Priorities   of data_attr

 | Constructor  of data_attr
 | Injective    of data_attr
 | Predicate    of data_attr

 | Functional   of data_attr
 | Function     of data_attr

 | Anywhere     of data_attr
 | Owise        of data_attr

 | Subsort      of data_attr
 | Projection   of data_attr
 | Initializer  of data_attr


 (* USELESS attributes
 | Topcellinit  of data_attr
 | Topcell      of data_attr
 | Cell         of data_attr
 | Maincell     of data_attr
 | Cellname     of data_attr
 | Cellfragment of data_attr
 | Celloptabst  of data_attr
 (* ... *)
 | Latex        of data_attr
 | Color        of data_attr
 | Colors       of data_attr
 | Prefer       of data_attr
 | Nothread     of data_attr
 | Hook         of data_attr

 | SMTlib       of data_attr
 | SMThook      of data_attr
 | Format       of data_attr

 | StartLine    of data_attr
 | StartCol     of data_attr

 | Token        of data_attr
 | Klabel       of data_attr
 | Terminals    of data_attr
 | Index        of data_attr

 | Keyword      of data_attr
 | Unique       of data_attr
 | Location     of data_attr
 | Source       of data_attr
 | Production   of data_attr

 | Element      of data_attr
 | Concat       of data_attr

 | Sortinject   of data_attr
 | Hasdomainval of data_attr *)

 | Other        of string * data_attr

type symbol = name * quant_var list * param list * param

type predicate =
 | Sym of name * param list * axiom list
 | Var of name * param
and axiom =
 | Equals of param list * axiom * axiom
 | Exists of param list * (name * param) * axiom
 | And of param list * axiom * axiom
 | Or  of param list * axiom * axiom
 | Not of param list * axiom
 | Implies of param list * axiom * axiom
 | Bottom of param list
 | Top of param list
 | Rewrites of param list * axiom * axiom
 | In of param list * (name * param) * axiom
 | Dom_val of sort * name
 | Predicate of predicate

type def = A of axiom | D of name * quant_var

type alias = symbol * (name * quant_var list * (name * param) list * def)

type kommand_aux =
 | Sort     of sort
 | H_sort   of sort
 | Symbol   of symbol
 | H_symbol of symbol
 | Alias    of alias
 | Axiom    of quant_var list * axiom

type kommand = kommand_aux * attribute list

type import = name * attribute list

type kmodule = name * import list * kommand list * attribute list

type file =
   F_sem  of attribute list * kmodule list
 | F_exec of axiom


(** Getter *)

(** For symbol *)

(* type symbol = name * quant_var list * param list * param *)

let get_name : symbol -> name = fun s ->
  let (n, _, _, _) = s in n

let has_no_param : symbol -> bool = fun s ->
  let (_, _, p_l, _) = s in
  match p_l with
  | [] -> true
  | _  -> false

let get_param : symbol -> param = fun s ->
  let (_, _, _, p) = s in p

let get_sort : symbol -> sort = fun s -> (* Fix TODO *)
  let p = get_param s in
  match p with
  | S s -> s
  | Q _ -> failwith "No sort"

(** [is_constructor s attr_l] returns:
      - None, if the attribut "constructor" is not in [attr_l]
      - the type of [s] if the attributs "constructor" and
                           "injecitve" are in [attr_l]
      - A warning if the attribut "constructor" is in [attr_l]
                     but not the attribut "injective" *)
let is_constructor : symbol -> attribute list -> sort option =
  fun s attri_l ->
  let rec aux l acc = match l with
   | []   -> acc
   | t::q -> match t with
            | Constructor _ -> aux q (true, snd acc)
            | Injective   _ -> aux q (fst acc, true)
            | _             -> aux q acc
  in
  let is_cons, is_inj = aux attri_l (false, false) in
  match is_cons, is_inj with
  | (false, _)     -> None
  | (true, true)   -> Some (get_sort s)
  | (true, false)  ->
     wrn_1 _STDOUT "WARNING The symbol (%s) is declared \
                    'constructor' but not 'injective'!" (get_name s) ;
     None



(** For axiom *)

let rec is_predicate : axiom -> bool = fun a ->
  match a with
  | Equals(_,a1,a2)  -> is_predicate a1 || is_predicate a2
  | Exists(_,_,a)    -> is_predicate a
  | And(_,a1,a2)     -> is_predicate a1 || is_predicate a2
  | Or(_,a1,a2)      -> is_predicate a1 || is_predicate a2
  | Not(_,a)         -> is_predicate a
  | Implies(_,a1,a2) -> is_predicate a1 || is_predicate a2
  | Bottom   _  -> false
  | Top      _  -> false
  | Rewrites _  -> false (* users' rule *)
  | In(_,_,a)        -> is_predicate a
  | Dom_val  _  -> false
  | Predicate p -> match p with
                   | Sym(n, _, _) -> (* @TODO (n,_,a_l) ? *)
                      begin
                       try
                         let res = String.sub n 0 5 in String.equal res "Lblis"
                       with _ -> false
                      end
                   | Var _ -> false

let is_rule : axiom -> bool = fun a ->
  match a with
  | Rewrites _ -> true
  | _ -> false

let is_conditional_rule : axiom -> bool = fun a ->
  match a with
  | Top _ -> false
  | _     -> true

let is_cooling_rule : attribute list -> bool = fun l ->
  let f a = match a with
    | Cool _ -> true
    | _ -> false
  in
  List.fold_left (fun acc x -> f x || acc) false l

let is_heating_rule : attribute list -> bool = fun l ->
  let f a = match a with
    | Heat _ -> true
    | _ -> false
  in
  List.fold_left (fun acc x -> f x || acc) false l
