open Common.Type
open Common.Count_data

type rewrite = { lhs : alias ; rhs : quant_var list * axiom }
type common_data = Format.formatter * count_data * attribute list
(* Main algorithm : compute the number of ... + create or not the dependency graph *)

(* BAD IDEA
let check_is_predicat (cd : count_data) (attr_l : attribute list) (acc : 'a)
      (ax : axiom) (* a bit redundant *)
      (f_rewrite        : attribute list -> 'a -> 'b -> 'a) (init_rewrite : 'b)
      (f_axiom          : attribute list -> 'a -> 'c -> 'a) (init_axiom : 'c)
    : 'a =
  if Axiom.is_rule_axiom ax
  then (incr_k_ax_rule         cd ; f_rewrite attr_l acc init_rewrite)
  else (incr_k_ax_without_attr cd ; f_axiom   attr_l acc init_axiom)
         if Axiom.is_predicate_axiom ax
      then (incr_k_ax_predicat cd ; f_ax_predicat acc (qv_l, ax))
      else (incr_k_ax_owise    cd ; f_ax_owise    acc (qv_l, ax))
*)

let axiom_cases (cd : count_data) (attr_l : attribute list) (curr_attr : attribute)
      (acc : 'a) (qv_l : quant_var list) (ax : axiom)
      (f_ax_default : attribute list -> 'a -> quant_var list * axiom -> 'a)
      ((f_ax_subsort       : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_predicat    : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_projection  : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_functional  : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_constructor : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_assoc       : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_comm        : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_idem        : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_unit        : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_initializer : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_owise       : attribute list -> 'a -> quant_var list * axiom -> 'a)) : 'a =
  (match curr_attr with
   | Subsort     _ ->
      incr_k_ax_subsort     cd ; f_ax_subsort attr_l acc (qv_l, ax)
   | Projection  _ ->
      incr_k_ax_projection  cd ; f_ax_projection attr_l acc (qv_l, ax)
   | Functional  _ ->
      incr_k_ax_functional  cd ; f_ax_functional attr_l acc (qv_l, ax)
   | Constructor _ ->
      incr_k_ax_constructor cd ; f_ax_constructor attr_l acc (qv_l, ax)
   | Assoc _ ->
      incr_k_ax_assoc cd ; f_ax_assoc attr_l acc (qv_l, ax)
   | Comm  _ ->
      incr_k_ax_comm  cd ; f_ax_comm attr_l acc (qv_l, ax)
   | Idem  _ ->
      incr_k_ax_idem  cd ; f_ax_idem attr_l acc (qv_l, ax)
   | Unit  _ ->
      incr_k_ax_unit  cd ; f_ax_unit attr_l acc (qv_l, ax)
   | Initializer _ ->
      incr_k_ax_initializer cd ; f_ax_initializer attr_l acc (qv_l, ax)
   | Owise       _ ->
      (* check_is_predicat cd [] acc ax f_ax_predicat (qv_l, ax) f_ax_owise (qv_l, ax) - @TODO *)
      if Axiom.is_predicate ax
      then (incr_k_ax_predicat cd ; f_ax_predicat attr_l acc (qv_l, ax))
      else (incr_k_ax_owise    cd ; f_ax_owise    attr_l acc (qv_l, ax))
   | _ -> (* Format.printf (Color.yel "WARNING: %s is the only one for a rule") attr * ; @TODO *)
      incr_k_ax_with_one_attr cd ; f_ax_default attr_l acc (qv_l, ax))

type 'a meta_axiom = attribute list -> 'a -> quant_var list -> axiom -> 'a

let meta_kommand_iter
      (meta_f_alias : kommand list -> attribute list -> 'a -> alias -> 'a)
      (meta_f_axiom : 'a meta_axiom -> 'a meta_axiom)
      (cd : count_data) (l : kommand list) (neutral_el : 'a)
      (f_sort           : attribute list -> 'a -> sort    -> 'a)
      (f_hooked_sort    : attribute list -> 'a -> sort    -> 'a)
      (f_symbol         : attribute list -> 'a -> symbol  -> 'a)
      (f_hooked_symbol  : attribute list -> 'a -> symbol  -> 'a)
      (f_alias          : attribute list -> 'a -> alias   -> 'a)
      (f_rewrite        : attribute list -> 'a -> 'b -> 'a)
      (f_ax_default     : attribute list -> 'a -> quant_var list * axiom -> 'a)
      ((f_ax_subsort       : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_predicat    : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_projection  : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_functional  : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_constructor : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_assoc       : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_comm        : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_idem        : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_unit        : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_initializer : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_owise       : attribute list -> 'a -> quant_var list * axiom -> 'a) as transformation)
      (f_each_end_iter : unit -> unit)
    : 'a =
  let g_attr : 'a meta_axiom = fun attr_l acc qv_l ax ->
    match attr_l with
    | [] -> (if Axiom.is_predicate ax
             then (incr_k_ax_predicat     cd ; f_ax_predicat  attr_l acc (qv_l, ax))
             else (incr_k_ax_without_attr cd ; f_ax_default attr_l acc (qv_l, ax)))
    | [attr] -> axiom_cases cd attr_l attr acc qv_l ax f_ax_default transformation
    | _ ->
       (incr_k_ax_several_attr cd ;
        (* Format.printf (yel "There is an axiom with more than one attribute.\n") ;
         * @TODO print the list *)
        f_ax_default attr_l acc (qv_l, ax))
  in
  let rec aux l acc = match l with
    | [] -> acc
    | (c, attr_l)::q ->
       let res = match c with
         | Sort     s -> incr_k_sort cd        ; f_sort attr_l acc s
         | H_sort   s -> incr_k_hooked_sort cd ; f_hooked_sort attr_l acc s
         | Symbol   s -> incr_k_symbol cd        ; f_symbol attr_l acc s
         | H_symbol s -> incr_k_hooked_symbol cd ; f_hooked_symbol attr_l acc s
         | Alias   al -> meta_f_alias q attr_l acc al
         | Axiom(qv_l, ax) -> incr_k_axiom cd ; meta_f_axiom g_attr attr_l acc qv_l ax
       in
       f_each_end_iter() ; aux q res
  in aux l neutral_el

let kommand_iter_without_alias
      (cd : count_data) (l : kommand list) (neutral_el : 'a)
      (f_sort           : attribute list -> 'a -> sort    -> 'a)
      (f_hooked_sort    : attribute list -> 'a -> sort    -> 'a)
      (f_symbol         : attribute list -> 'a -> symbol  -> 'a)
      (f_hooked_symbol  : attribute list -> 'a -> symbol  -> 'a)
      (f_alias          : attribute list -> 'a -> alias   -> 'a)
      (f_rewrite        : attribute list -> 'a -> 'b -> 'a)
      (f_ax_default     : attribute list -> 'a -> quant_var list * axiom -> 'a)
      ((f_ax_subsort       : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_predicat    : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_projection  : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_functional  : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_constructor : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_assoc       : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_comm        : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_idem        : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_unit        : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_initializer : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_owise       : attribute list -> 'a -> quant_var list * axiom -> 'a) as transformation)
      (f_each_end_iter : unit -> unit)
    : 'a
  =
  let meta_f_alias q attr_l acc al = match q with
    | [] -> (incr_k_alias cd ; f_alias attr_l acc al)
    | h::_ ->
       match h with
       | Axiom(qv_l, ax), attr_l_ax ->
          let xattr_l = attr_l@attr_l_ax in
          if Axiom.is_rule ax
          then (incr_k_ax_rule cd ; f_rewrite xattr_l acc { lhs = al ; rhs = (qv_l, ax) })
          else (incr_k_alias   cd ; f_alias xattr_l acc al)
       | _  -> (incr_k_alias cd ; f_alias attr_l acc al)
  in
  let meta_f_axiom g_attr attr_l acc qv_l ax = g_attr attr_l acc qv_l ax in
  meta_kommand_iter meta_f_alias meta_f_axiom cd l neutral_el f_sort f_hooked_sort
    f_symbol f_hooked_symbol f_alias f_rewrite f_ax_default transformation f_each_end_iter

let kommand_iter_with_alias
      (cd : count_data) (l : kommand list) (neutral_el : 'a)
      (f_sort           : attribute list -> 'a -> sort    -> 'a)
      (f_hooked_sort    : attribute list -> 'a -> sort    -> 'a)
      (f_symbol         : attribute list -> 'a -> symbol  -> 'a)
      (f_hooked_symbol  : attribute list -> 'a -> symbol  -> 'a)
      (f_alias          : attribute list -> 'a -> alias   -> 'a)
      (f_rewrite        : attribute list -> 'a -> 'b -> 'a)
      (f_ax_default     : attribute list -> 'a -> quant_var list * axiom -> 'a)
      ((f_ax_subsort       : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_predicat    : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_projection  : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_functional  : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_constructor : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_assoc       : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_comm        : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_idem        : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_unit        : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_initializer : attribute list -> 'a -> quant_var list * axiom -> 'a)
       , (f_ax_owise       : attribute list -> 'a -> quant_var list * axiom -> 'a) as transformation)
      (f_each_end_iter : unit -> unit)
    : 'a
  =
  let meta_f_alias _ attr_l acc al = incr_k_alias cd ; f_alias attr_l acc al in
  let meta_f_axiom g_attr attr_l acc qv_l ax =
    if Axiom.is_rule ax
    then (incr_k_ax_rule cd ; f_rewrite attr_l acc (qv_l, ax))
    else g_attr attr_l acc qv_l ax
  in
  meta_kommand_iter meta_f_alias meta_f_axiom cd l neutral_el f_sort f_hooked_sort
    f_symbol f_hooked_symbol f_alias f_rewrite f_ax_default transformation f_each_end_iter
