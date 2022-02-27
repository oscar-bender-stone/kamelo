
open Common.Type

let trash =
  [ "Lbl'Hash'if'UndsHash'then'UndsHash'else'UndsHash'fi'Unds'K-EQUAL-SYNTAX'Unds'Sort'Unds'Bool'Unds'Sort'Unds'Sort" ;
    "LblBase2String'LParUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'Int'Unds'Int" ;
    "LblFloat2String'LParUndsRParUnds'STRING-COMMON'Unds'String'Unds'Float" ;
    "LblfindChar'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'String'Unds'Int" ;
    "LblfreshId'LParUndsRParUnds'ID-COMMON'Unds'Id'Unds'Int" ;
    "LblrfindChar'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'String'Unds'Int" ;
    "LblinitKCell" ]

let to_delete x =
  List.fold_left (fun acc v_test -> acc || (x = v_test)) false trash

  (*
let has_generated_SortId : bool ref = ref false (* "SortId" *)

let is_SortId x = if x = "SortId" then has_generated_SortId := true

   *)
(*

type name = string
type sort = name
type quant_var = name
type param = S of sort | Q of quant_var

type data_attr = param list * string list

type symbol = name * quant_var list * param list * param


let f_sort = fun s acc ->
  is_SortId s ;
  if to_delete x then acc else


    let f_sym  = fun _ _ s _ -> in
    let
        f_sort f_sort f_sym f_sym

type import = name * attribute list

type kmodule = name * import list * kommand list * attribute list

type file =
 | F_sem  of attribute list * kmodule list
 | F_exec of axiom
 *)

(** [cleaning l] deletes unused symbols listed in the list [trash]. *)
let cleaning : kommand list -> kommand list = fun k_l ->
  let rec aux_ax ax = match ax with
    | Equals(p_l, ax1, ax2)      -> Equals(p_l, aux_ax ax1, aux_ax ax2)
    | Exists(p_l, (n, p), ax)    -> Exists(p_l, (n, p),  aux_ax ax)
    | And(p_l, ax1, ax2)         -> And(p_l, aux_ax ax1, aux_ax ax2)
    | Or(p_l, ax1, ax2)          -> Or (p_l, aux_ax ax1, aux_ax ax2)
    | Not(p_l, ax)               -> Not(p_l, aux_ax ax)
    | Implies(p_l, ax1, ax2)     -> Implies(p_l, aux_ax ax1, aux_ax ax2)
    | Bottom _                   -> ax
    | Top _                      -> ax
    | Rewrites(p_l, ax1, ax2)    -> Rewrites(p_l, aux_ax ax1, aux_ax ax2)
    | In(p_l, (n, p), ax)        -> In(p_l, (n, p), aux_ax ax)
    | Dom_val(_, _)              -> ax (* TODO fix ? *)
    | Predicate(Sym(n,p_l,ax_l)) -> (if to_delete n then
                                       assert false
                                     else
                                       Predicate(Sym(n,p_l,List.map aux_ax ax_l)))
    | Predicate(Var(_, _))       -> ax
  in
    (*
type predicate =
 | Sym of name * param list * axiom list
 | Var of name * param

type def = A of axiom | D of name * quant_var

type alias = symbol * (name * quant_var list * (name * param) list * def)
     *)
  (* let f_sym n kommand =
    if to_delete n then assert false
    else
      if not(!has_generated_SortId)
      then (has_generated_SortId := true ; (Sort "SortId", [])::[kommand])
      else [kommand]
  in
   *)
  let rec aux : kommand list -> kommand list = function
    | []   -> []
    | kommand::q ->
       (try
          (match kommand with
           | Sort "SortId"              ,_ -> []
           | Sort _                     ,_ -> [kommand]
           | H_sort _                   ,_ -> [kommand]
           | Symbol   (n,_,_,_)         ,_ -> if to_delete n then assert false else [kommand]
           | H_symbol (n,_,_,_)         ,_ -> if to_delete n then assert false else [kommand]
           | Alias (s,(n,qv_l,l, A ax)) ,a -> [(Alias (s,(n,qv_l,l, A (aux_ax ax))), a)]
           | Alias (_,(_,_,_, D (n,_))) ,_ -> if to_delete n then assert false else [kommand]
           | Axiom(qv_l, ax)            ,a -> [(Axiom(qv_l, aux_ax ax), a)])@(aux q)
        with _ -> aux q)
  in
  (* let n, i_l, k_l, attr_l_f = f in (n, i_l, aux k_l, attr_l_f) *)
  aux k_l
  (*
  match f with
  | F_sem(attr_l, (n, i_l, k_l, attr_l_f)) -> (attr_l, (n, i_l, aux k_l, attr_l_f))
  | F_exec _ -> InternalError "An executable can't be cleaning." *)

(*

type kommand = kommand_aux * attribute list

type kmodule = name * import list * kommand list * attribute list *)
