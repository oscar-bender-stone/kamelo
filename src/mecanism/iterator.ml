open Common.Type
open Common.Getter
open Common.Error

open Count_data

(* type rewrite = { lhs : alias ; rhs : quant_var list * axiom }
type common_data = Format.formatter * count_data * attribute list *)

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

type ('a, 's) meta_axiom = attribute list -> 'a -> 's -> quant_var list * axiom -> ('a * 's)

(** [axiom_cases cd attr_l curr_attr acc sign qv_l ax f_exists f_equals f_or_bottom f_not f_implies]
    acc ~ extra_data + sign ~ signature *)
let axiom_cases
      (cd : count_data) (attr_l : attribute list) (curr_attr : attribute option)
      (acc : 'a) (sign : 's) (qv_l : quant_var list) (ax : axiom)
      ((f_exists_ax_subsort    : ('a, 's) meta_axiom),
       (f_exists_ax_functional : ('a, 's) meta_axiom))
      ((f_equals_ax_assoc : ('a, 's) meta_axiom),
       (f_equals_ax_comm  : ('a, 's) meta_axiom),
       (f_equals_ax_idem  : ('a, 's) meta_axiom),
       (f_equals_ax_unit  : ('a, 's) meta_axiom))
      ((f_or_bottom_ax_constructor : ('a, 's) meta_axiom))
      ((f_not_ax_constructor : ('a, 's) meta_axiom))
      ((f_implies_ax_constructor     : ('a, 's) meta_axiom),
       (f_implies_ax_initializer     : ('a, 's) meta_axiom),
       (f_implies_ax_projection      : ('a, 's) meta_axiom),
       (f_implies_ax_predicate_true  : ('a, 's) meta_axiom),
       (f_implies_ax_predicate_false : ('a, 's) meta_axiom), (* [owise] *)
       (f_implies_ax_owise           : ('a, 's) meta_axiom),
       (f_implies_ax_default         : ('a, 's) meta_axiom)) : ('a * 's) =
  match ax with
  | Exists _ -> incr_k_exists_ax cd ;
     (match curr_attr with
      | Some (Subsort _) ->
         incr_k_ax_subsort     cd ; f_exists_ax_subsort attr_l acc sign (qv_l, ax)
      | Some (Functional  _) ->
         incr_k_ax_functional  cd ; f_exists_ax_functional attr_l acc sign (qv_l, ax)
      | _ -> raise (InternalError "Need to update [axiom_cases], case Exists."))
  | Equals _ -> incr_k_equals_ax cd ;
     (match curr_attr with
      | Some (Assoc _) ->
         incr_k_ax_assoc cd ; f_equals_ax_assoc attr_l acc sign (qv_l, ax)
      | Some (Comm  _) ->
         incr_k_ax_comm  cd ; f_equals_ax_comm attr_l acc sign (qv_l, ax)
      | Some (Idem  _) ->
         incr_k_ax_idem  cd ; f_equals_ax_idem attr_l acc sign (qv_l, ax)
      | Some (Unit  _) ->
         incr_k_ax_unit  cd ; f_equals_ax_unit attr_l acc sign (qv_l, ax)
      | _ -> raise (InternalError "Need to update [axiom_cases], case Equals."))
  | Or _ -> incr_k_or_ax cd ;
     (match curr_attr with
      | Some (Constructor _) ->
         incr_k_or_ax_junk_constructor cd ; f_or_bottom_ax_constructor attr_l acc sign (qv_l, ax)
      | _ -> raise (InternalError "Need to update [axiom_cases], case Or."))
  | Bottom _ -> incr_k_bottom_ax cd ;
     (match curr_attr with
      | Some (Constructor _) ->
         incr_k_bottom_ax_junk_constructor cd ; f_or_bottom_ax_constructor attr_l acc sign (qv_l, ax)
      | _ -> raise (InternalError "Need to update [axiom_cases], case Bottom."))
  | Not _ -> incr_k_not_ax cd ;
     (match curr_attr with
      | Some (Constructor _) ->
         incr_k_not_ax_diff_constructor cd ; f_not_ax_constructor attr_l acc sign (qv_l, ax)
      | _ -> raise (InternalError "Need to update [axiom_cases], case Not."))
  | Implies _ -> incr_k_implies_ax cd ;
     (match curr_attr with
      | Some (Constructor _) ->
         incr_k_ax_same_constructor cd ; f_implies_ax_constructor attr_l acc sign (qv_l, ax)
      | Some (Initializer _) ->
         incr_k_ax_initializer cd ; f_implies_ax_initializer attr_l acc sign (qv_l, ax)
      | Some (Projection  _) ->
         incr_k_ax_projection  cd ; f_implies_ax_projection attr_l acc sign (qv_l, ax)
      | Some (Owise _) ->
         if is_predicate ax
         then (incr_k_ax_predicate_false cd ; f_implies_ax_predicate_false attr_l acc sign (qv_l, ax))
         else (incr_k_ax_owise    cd ; f_implies_ax_owise attr_l acc sign (qv_l, ax))
      | None ->
         if is_predicate ax
         then (incr_k_ax_predicate_true cd ; f_implies_ax_predicate_true attr_l acc sign (qv_l, ax))
         else (incr_k_ax_without_attr cd ; f_implies_ax_default attr_l acc sign (qv_l, ax))
      | _ ->
         if is_predicate ax
         then (incr_k_ax_predicate_true cd ; f_implies_ax_predicate_true attr_l acc sign (qv_l, ax))
         else (incr_k_ax_with_one_attr cd ; f_implies_ax_default attr_l acc sign (qv_l, ax)))
        (* raise (InternalError "Need to update [axiom_cases], case Implies.")) *)
  | Rewrites _ -> (acc, sign) (* raise (InternalError "Rewriting translation not possible.") *)
    (* Ici, on pourrait s'attendre à renvoyer une erreur.
       C'est ce qu'il faudrait faire si [kommand_iter_with_alias] passait par là.
       Mais dans le cas de [kommand_iter_without_alias], cet axiome a déjà été traduit via [rewriting_cases].
       Nous préférons donc ne rien faire dans ce cas de figure. *)
  | _ -> raise (InternalError "Need to update [axiom_cases], case root.")

(** [rewriting_cases cd attr_l curr_attr acc sign qv_l ax f_heating f_cooling f_semantic]
    acc ~ extra_data + sign ~ signature *)
let rewriting_cases
      (cd : count_data) (attr_l : attribute list) (acc : 'a) (sign : 's)
      (al : alias option) (qv_l : quant_var list) (ax : axiom)
      ((f_heating  : attribute list -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)),
       (f_cooling  : attribute list -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)),
       (f_semantic : attribute list -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's))) : ('a * 's) =
  incr_k_rewriting_ax cd ;
  let al = match al with
    | None -> raise (InternalError "Need to update [rewriting_cases].")
    | Some al -> al
  in
  match attr_l with
  | [Heat _] -> incr_k_ax_heating  cd ; f_heating  attr_l acc sign al (qv_l, ax)
  | [Cool _] -> incr_k_ax_cooling  cd ; f_cooling  attr_l acc sign al (qv_l, ax)
  | [Owise _] -> incr_k_ax_semantic cd ; f_semantic attr_l acc sign al (qv_l, ax)
  | [] -> incr_k_ax_semantic cd ; f_semantic attr_l acc sign al (qv_l, ax)
  | _ -> raise (InternalError "Need to update [rewriting_cases].")

let meta_kommand_iter
      (meta_f_alias : kommand list -> attribute list -> 'a -> 's -> alias -> ('a * 's))
      (meta_f_axiom : ('a, 's) meta_axiom -> ('a, 's) meta_axiom)
      (cd : count_data) (l : kommand list) (neutral_el : 'a) (init_sign : 's)
      (f_sort           : attribute list -> 'a -> 's -> sort    -> ('a * 's))
      (f_hooked_sort    : attribute list -> 'a -> 's -> sort    -> ('a * 's))
      (f_symbol         : attribute list -> 'a -> 's -> symbol  -> ('a * 's))
      (f_hooked_symbol  : attribute list -> 'a -> 's -> symbol  -> ('a * 's))
      (f_alias          : attribute list -> 'a -> 's -> alias   -> ('a * 's))
      (f_ax_default     : ('a, 's) meta_axiom)
      ((f_exists_ax_subsort    : ('a, 's) meta_axiom),
       (f_exists_ax_functional : ('a, 's) meta_axiom)       as f_exists)
      ((f_equals_ax_assoc : ('a, 's) meta_axiom),
       (f_equals_ax_comm  : ('a, 's) meta_axiom),
       (f_equals_ax_idem  : ('a, 's) meta_axiom),
       (f_equals_ax_unit  : ('a, 's) meta_axiom)            as f_equals)
      ((f_or_bottom_ax_constructor : ('a, 's) meta_axiom)   as f_or_bottom)
      ((f_not_ax_constructor : ('a, 's) meta_axiom)         as f_not)
      ((f_implies_ax_constructor     : ('a, 's) meta_axiom),
       (f_implies_ax_initializer     : ('a, 's) meta_axiom),
       (f_implies_ax_projection      : ('a, 's) meta_axiom),
       (f_implies_ax_predicate_true  : ('a, 's) meta_axiom),
       (f_implies_ax_predicate_false : ('a, 's) meta_axiom),
       (f_implies_ax_owise           : ('a, 's) meta_axiom),
       (f_implies_ax_default         : ('a, 's) meta_axiom) as f_implies)
      (f_each_end_iter : unit -> unit)
    : ('a * 's) =
  let g_attr : ('a, 's) meta_axiom = fun attr_l acc sign (qv_l, ax) ->
    match attr_l with
    | [] -> (if is_predicate ax
             then (incr_k_implies_ax cd ; incr_k_ax_predicate_true cd ; f_implies_ax_predicate_true attr_l acc sign (qv_l, ax)) (* TODO Fix? *)
             else (axiom_cases cd attr_l None acc sign qv_l ax f_exists f_equals f_or_bottom f_not f_implies)) (* f_ax_default attr_l acc (qv_l, ax)) *)
    | [attr] -> axiom_cases cd attr_l (Some attr) acc sign qv_l ax f_exists f_equals f_or_bottom f_not f_implies
    | _ ->
       (incr_k_ax_several_attr cd ;
        (* wrn_msg "There is an axiom with more than one attribute." ;
           @TODO print the list *)
        f_ax_default attr_l acc sign (qv_l, ax))
  in
  let rec aux l (acc, sign) = match l with
    | [] -> (acc, sign)
    | (c, attr_l)::q ->
       let res = match c with
         | Sort     s -> incr_k_sort cd        ; f_sort attr_l acc sign s
         | H_sort   s -> incr_k_hooked_sort cd ; f_hooked_sort attr_l acc sign s
         | Symbol   s -> incr_k_symbol cd        ; f_symbol attr_l acc sign s
         | H_symbol s -> incr_k_hooked_symbol cd ; f_hooked_symbol attr_l acc sign s
         | Alias   al -> meta_f_alias q attr_l acc sign al
         | Axiom(qv_l, ax) -> incr_k_axiom cd ; meta_f_axiom g_attr attr_l acc sign (qv_l, ax)
       in
       f_each_end_iter () ; aux q res
  in aux l (neutral_el, init_sign)

let kommand_iter_without_alias
      (cd : count_data) (l : kommand list) (neutral_el : 'a) (init_sign : 's)
      (f_sort           : attribute list -> 'a -> 's -> sort    -> ('a * 's))
      (f_hooked_sort    : attribute list -> 'a -> 's -> sort    -> ('a * 's))
      (f_symbol         : attribute list -> 'a -> 's -> symbol  -> ('a * 's))
      (f_hooked_symbol  : attribute list -> 'a -> 's -> symbol  -> ('a * 's))
      (f_alias          : attribute list -> 'a -> 's -> alias   -> ('a * 's))
      ((f_rewrites_ax_heating  : attribute list -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)),
       (f_rewrites_ax_cooling  : attribute list -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)),
       (f_rewrites_ax_semantic : attribute list -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)) as f_rewrites)
      (f_ax_default     : ('a, 's) meta_axiom)
      ((f_exists_ax_subsort    : ('a, 's) meta_axiom),
       (f_exists_ax_functional : ('a, 's) meta_axiom)       as f_exists)
      ((f_equals_ax_assoc : ('a, 's) meta_axiom),
       (f_equals_ax_comm  : ('a, 's) meta_axiom),
       (f_equals_ax_idem  : ('a, 's) meta_axiom),
       (f_equals_ax_unit  : ('a, 's) meta_axiom)            as f_equals)
      ((f_or_bottom_ax_constructor : ('a, 's) meta_axiom)   as f_or_bottom)
      ((f_not_ax_constructor : ('a, 's) meta_axiom)         as f_not)
      ((f_implies_ax_constructor     : ('a, 's) meta_axiom),
       (f_implies_ax_initializer     : ('a, 's) meta_axiom),
       (f_implies_ax_projection      : ('a, 's) meta_axiom),
       (f_implies_ax_predicate_true  : ('a, 's) meta_axiom),
       (f_implies_ax_predicate_false : ('a, 's) meta_axiom),
       (f_implies_ax_owise           : ('a, 's) meta_axiom),
       (f_implies_ax_default         : ('a, 's) meta_axiom) as f_implies)
      (f_each_end_iter : unit -> unit) : ('a * 's) =
  let meta_f_alias q attr_l acc sign al = match q with
    | [] -> (incr_k_alias cd ; f_alias attr_l acc sign al)
    | h::_ ->
       match h with
       | Axiom(qv_l, ax), attr_l_ax ->
          let xattr_l = attr_l@attr_l_ax in
          if is_rule ax
          then rewriting_cases cd xattr_l acc sign (Some al) qv_l ax f_rewrites
          else (incr_k_alias cd ; f_alias xattr_l acc sign al)
       | _  -> (incr_k_alias cd ; f_alias attr_l  acc sign al)
  in
  let meta_f_axiom g_attr attr_l acc sign (qv_l, ax) =
    g_attr attr_l acc sign (qv_l, ax)
  in
  meta_kommand_iter meta_f_alias meta_f_axiom cd l neutral_el init_sign f_sort f_hooked_sort
    f_symbol f_hooked_symbol f_alias f_ax_default
    f_exists f_equals f_or_bottom f_not f_implies f_each_end_iter

let kommand_iter_with_alias
      (cd : count_data) (l : kommand list) (neutral_el : 'a) (init_sign : 's)
      (f_sort           : attribute list -> 'a -> 's -> sort    -> ('a * 's))
      (f_hooked_sort    : attribute list -> 'a -> 's -> sort    -> ('a * 's))
      (f_symbol         : attribute list -> 'a -> 's -> symbol  -> ('a * 's))
      (f_hooked_symbol  : attribute list -> 'a -> 's -> symbol  -> ('a * 's))
      (f_alias          : attribute list -> 'a -> 's -> alias   -> ('a * 's))
      ((f_rewrites_ax_heating  : attribute list -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)),
       (f_rewrites_ax_cooling  : attribute list -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)),
       (f_rewrites_ax_semantic : attribute list -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)) as f_rewrites)
      (f_ax_default     : ('a, 's) meta_axiom)
      ((f_exists_ax_subsort    : ('a, 's) meta_axiom),
       (f_exists_ax_functional : ('a, 's) meta_axiom)       as f_exists)
      ((f_equals_ax_assoc : ('a, 's) meta_axiom),
       (f_equals_ax_comm  : ('a, 's) meta_axiom),
       (f_equals_ax_idem  : ('a, 's) meta_axiom),
       (f_equals_ax_unit  : ('a, 's) meta_axiom)            as f_equals)
      ((f_or_bottom_ax_constructor : ('a, 's) meta_axiom)   as f_or_bottom)
      ((f_not_ax_constructor : ('a, 's) meta_axiom)         as f_not)
      ((f_implies_ax_constructor     : ('a, 's) meta_axiom),
       (f_implies_ax_initializer     : ('a, 's) meta_axiom),
       (f_implies_ax_projection      : ('a, 's) meta_axiom),
       (f_implies_ax_predicate_true  : ('a, 's) meta_axiom),
       (f_implies_ax_predicate_false : ('a, 's) meta_axiom),
       (f_implies_ax_owise           : ('a, 's) meta_axiom),
       (f_implies_ax_default         : ('a, 's) meta_axiom) as f_implies)
      (f_each_end_iter : unit -> unit) : ('a * 's) =
  let meta_f_alias _ attr_l acc sign al =
    incr_k_alias cd ; f_alias attr_l acc sign al
  in
  let meta_f_axiom g_attr attr_l acc sign (qv_l, ax) =
    if is_rule ax
    then rewriting_cases cd attr_l acc sign None qv_l ax f_rewrites
    else g_attr attr_l acc sign (qv_l, ax)
  in
  meta_kommand_iter meta_f_alias meta_f_axiom cd l neutral_el init_sign f_sort f_hooked_sort
    f_symbol f_hooked_symbol f_alias f_ax_default
    f_exists f_equals f_or_bottom f_not f_implies f_each_end_iter
