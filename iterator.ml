open Type
open Count_data

type rewrite = { lhs : alias ; rhs : quant_var list * axiom }
type common_data = Format.formatter * count_data * attribute list
(* Main algorithm : compute the number of ... + create or not the dependency graph *)

let kore_command_iter (cd : count_data) (l : command list) (neutral_el : 'a)
      (f_sort :          attribute list -> 'a -> sort    -> 'a)
      (f_hooked_sort :   attribute list -> 'a -> sort    -> 'a)
      (f_symbol :        attribute list -> 'a -> symbol  -> 'a)
      (f_hooked_symbol : attribute list -> 'a -> symbol  -> 'a)
      (f_alias :         attribute list -> 'a -> alias   -> 'a)
      (f_rewrite :       attribute list -> 'a -> rewrite -> 'a)
      (f_axiom :         attribute list -> 'a -> quant_var list * axiom -> 'a) : 'a =
  let rec aux l acc = match l with
    | [] -> acc
    | (c, attr_l)::q ->
       let res = match c with
         | Sort     s -> incr_k_sort cd        ; f_sort attr_l acc s
         | H_sort   s -> incr_k_hooked_sort cd ; f_hooked_sort attr_l acc s
         | Symbol   s -> incr_k_symbol cd        ; f_symbol attr_l acc s
         | H_symbol s -> incr_k_hooked_symbol cd ; f_hooked_symbol attr_l acc s
         | Alias al ->
            (match q with
             | [] -> (incr_k_alias cd ; f_alias attr_l acc al)
             | h::_ ->
                (match h with
                 | Axiom(qv_l, ax), attr_l_ax ->
                    let xattr_l = attr_l@attr_l_ax in
                    if Axiom.is_rule_axiom ax
                    then (incr_k_rule  cd ; f_rewrite xattr_l acc { lhs = al ; rhs = (qv_l, ax) })
                    else (incr_k_alias cd ; f_alias xattr_l acc al)
                 | _  -> (incr_k_alias cd ; f_alias  attr_l acc al)))
         | Axiom(qv_l, ax) -> (incr_k_axiom cd ; f_axiom attr_l acc (qv_l, ax))
       in
       aux q res
  in aux l neutral_el

let kore_command_iter_bis (cd : count_data) (l : command list) (neutral_el : 'a)
      (f_sort           : attribute list -> 'a -> sort    -> 'a)
      (f_hooked_sort    : attribute list -> 'a -> sort    -> 'a)
      (f_symbol         : attribute list -> 'a -> symbol  -> 'a)
      (f_hooked_symbol  : attribute list -> 'a -> symbol  -> 'a)
      (f_alias          : attribute list -> 'a -> alias   -> 'a)
      (f_rewrite        : attribute list -> 'a -> quant_var list * axiom -> 'a)
      (f_axiom          : attribute list -> 'a -> quant_var list * axiom -> 'a)
      (f_ax_subsort     : attribute list -> 'a -> quant_var list * axiom -> 'a)
      (f_ax_predicat    : attribute list -> 'a -> quant_var list * axiom -> 'a)
      (f_ax_projection  : attribute list -> 'a -> quant_var list * axiom -> 'a)
      (f_ax_functional  : attribute list -> 'a -> quant_var list * axiom -> 'a)
      (f_ax_constructor : attribute list -> 'a -> quant_var list * axiom -> 'a)
      (f_ax_assoc       : attribute list -> 'a -> quant_var list * axiom -> 'a)
      (f_ax_comm        : attribute list -> 'a -> quant_var list * axiom -> 'a)
      (f_ax_idem        : attribute list -> 'a -> quant_var list * axiom -> 'a)
      (f_ax_unit        : attribute list -> 'a -> quant_var list * axiom -> 'a)
      (f_ax_initializer : attribute list -> 'a -> quant_var list * axiom -> 'a)
      (f_ax_owise       : attribute list -> 'a -> quant_var list * axiom -> 'a)
    : 'a =
  let rec aux l acc = match l with
    | [] -> acc
    | (c, attr_l)::q ->
       let res = match c with
         | Sort     s -> incr_k_sort cd        ; f_sort attr_l acc s
         | H_sort   s -> incr_k_hooked_sort cd ; f_hooked_sort attr_l acc s
         | Symbol   s -> incr_k_symbol cd        ; f_symbol attr_l acc s
         | H_symbol s -> incr_k_hooked_symbol cd ; f_hooked_symbol attr_l acc s
         | Alias   al -> incr_k_alias cd ; f_alias attr_l acc al
         | Axiom(qv_l, ax) ->
            incr_k_axiom cd ;
            (match attr_l with
             | [Subsort     _] ->
                incr_k_ax_subsort     cd ; f_ax_subsort attr_l acc (qv_l, ax)
             | [Projection  _] ->
                incr_k_ax_projection  cd ; f_ax_projection attr_l acc (qv_l, ax)
             | [Functional  _] ->
                incr_k_ax_functional  cd ; f_ax_functional attr_l acc (qv_l, ax)
             | [Constructor _] ->
                incr_k_ax_constructor cd ; f_ax_constructor attr_l acc (qv_l, ax)
             | [Assoc _] ->
                incr_k_ax_assoc cd ; f_ax_assoc attr_l acc (qv_l, ax)
             | [Comm  _] ->
                incr_k_ax_comm  cd ; f_ax_comm attr_l acc (qv_l, ax)
             | [Idem  _] ->
                incr_k_ax_idem  cd ; f_ax_idem attr_l acc (qv_l, ax)
             | [Unit  _] ->
                incr_k_ax_unit  cd ; f_ax_unit attr_l acc (qv_l, ax)
             | [Initializer _] ->
                incr_k_ax_initializer cd ; f_ax_initializer attr_l acc (qv_l, ax)
             | [Owise       _] ->
                if Axiom.is_predicate_axiom ax
                then (incr_k_ax_predicat cd ; f_ax_predicat attr_l acc (qv_l, ax))
                else (incr_k_ax_owise    cd ; f_ax_owise    attr_l acc (qv_l, ax))
             | [] ->
                if Axiom.is_rule_axiom ax
                then (incr_k_ax_rule         cd ; f_rewrite attr_l acc (qv_l, ax))
                else (incr_k_ax_without_attr cd ; f_axiom   attr_l acc (qv_l, ax))
             | _  ->
                (incr_k_ax_several_attr cd ;
                 (*Format.printf (yel "There is an axiom with more than one attribute.\n") ; @TODO print the list *)
                 f_axiom attr_l acc (qv_l, ax)))
       in
       aux q res
  in aux l neutral_el
