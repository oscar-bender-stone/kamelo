
open Common.Type
open Interface.K_prelude

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

(** [cleaning k_l] deletes unused symbols listed in the list [trash]. *)
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
    | Ceil(_, _)                 -> ax (* TODO fix ? *)
    | Predicate(Sym(n,p_l,ax_l)) -> (if to_delete n then
                                       assert false
                                     else
                                       Predicate(Sym(n,p_l,List.map aux_ax ax_l)))
    | Predicate(Var(_, _))       -> ax
  in
  let rec aux : kommand list -> kommand list = function
    | []   -> []
    | kommand::q ->
       (try
          (match kommand with
           | Sort s                     ,_ -> if s = _SORT_ID then [] else [kommand]
           | H_sort _                   ,_ -> [kommand]
           | Symbol   (n,_,_,_)         ,_ -> if to_delete n then assert false else [kommand]
           | H_symbol (n,_,_,_)         ,_ -> if to_delete n then assert false else [kommand]
           | Alias (s,(n,qv_l,l, A ax)) ,a -> [(Alias (s,(n,qv_l,l, A (aux_ax ax))), a)]
           | Alias (_,(_,_,_, D (n,_))) ,_ -> if to_delete n then assert false else [kommand]
           | Axiom(qv_l, ax)            ,a -> [(Axiom(qv_l, aux_ax ax), a)])@(aux q)
        with _ -> aux q)
  in
  aux k_l
