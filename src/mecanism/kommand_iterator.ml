open Common.Type
open Common.Getter
open Common.Error

open Count_data

(* type rewrite = { lhs : alias ; rhs : quant_var list * axiom }
   type common_data = Format.formatter * count_data * attribute list *)

type ('a, 's) meta_axiom = data -> 'a -> 's -> quant_var list * axiom -> ('a * 's)

(** [axiom_cases cd data curr_attr acc sign qv_l ax f_exists f_equals f_or_bottom f_not f_implies]
    acc ~ extra_data + sign ~ signature *)
let axiom_cases
      (cd : count_data) (data : data) (curr_attr : attribute option)
      (acc : 'a) (sign : 's) (qv_l : quant_var list) (ax : axiom)
      ((f_exists_ax_subsort    : ('a, 's) meta_axiom),
       (f_exists_ax_functional : ('a, 's) meta_axiom))
      ((f_equals_ax_assoc   : ('a, 's) meta_axiom),
       (f_equals_ax_comm    : ('a, 's) meta_axiom),
       (f_equals_ax_idem    : ('a, 's) meta_axiom),
       (f_equals_ax_unit    : ('a, 's) meta_axiom),
       (f_equals_ax_default : ('a, 's) meta_axiom))
      ((f_or_bottom_ax_constructor : ('a, 's) meta_axiom))
      ((f_not_ax_constructor : ('a, 's) meta_axiom))
      ((f_implies_ax_constructor     : ('a, 's) meta_axiom),
       (f_implies_ax_initializer     : ('a, 's) meta_axiom),
       (f_implies_ax_projection      : ('a, 's) meta_axiom),
       (f_implies_ax_predicate_true  : ('a, 's) meta_axiom),
       (f_implies_ax_predicate_false : ('a, 's) meta_axiom), (* [owise] *)
       (f_implies_ax_owise           : ('a, 's) meta_axiom),
       (f_implies_ax_default         : ('a, 's) meta_axiom)) : ('a * 's) =
  let pos = snd data in match ax with
  | Exists _ -> incr_k_exists_ax cd ;
     (match curr_attr with
      | Some (Subsort _) ->
         incr_k_ax_subsort     cd ;
         (try f_exists_ax_subsort data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | Some (Functional  _) ->
         incr_k_ax_functional  cd ;
         (try f_exists_ax_functional data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | _ -> raise (InternalError "Need to update [axiom_cases], case Exists."))
  | Equals _ -> incr_k_equals_ax cd ;
     (match curr_attr with
      | Some (Assoc _) ->
         incr_k_ax_assoc cd ;
         (try f_equals_ax_assoc data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | Some (Comm  _) ->
         incr_k_ax_comm  cd ;
         (try f_equals_ax_comm data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | Some (Idem  _) ->
         incr_k_ax_idem  cd ;
         (try f_equals_ax_idem data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | Some (Unit  _) ->
         incr_k_ax_unit  cd ;
         (try f_equals_ax_unit data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | Some (Simpl _) -> (* Some (Simplification  _) - Axiome du prélude  *)
         incr_k_ax_without_attr cd ;
         (try f_equals_ax_default data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | None -> (* Some (Simplification  _) - Axiome du prélude  *)
         incr_k_ax_without_attr cd ;
         (try f_equals_ax_default data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | _ -> raise (InternalError "Need to update [axiom_cases], case Equals."))
  | Or _ -> incr_k_or_ax cd ;
     (match curr_attr with
      | Some (Constructor _) ->
         incr_k_or_ax_junk_constructor cd ;
         (try f_or_bottom_ax_constructor data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | _ -> raise (InternalError "Need to update [axiom_cases], case Or."))
  | Bottom _ -> incr_k_bottom_ax cd ;
     (match curr_attr with
      | Some (Constructor _) ->
         incr_k_bottom_ax_junk_constructor cd ;
         (try f_or_bottom_ax_constructor data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | _ -> raise (InternalError "Need to update [axiom_cases], case Bottom."))
  | Not _ -> incr_k_not_ax cd ;
     (match curr_attr with
      | Some (Constructor _) ->
         incr_k_not_ax_diff_constructor cd ;
         (try f_not_ax_constructor data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | _ -> raise (InternalError "Need to update [axiom_cases], case Not."))
  | Implies _ -> incr_k_implies_ax cd ;
     (match curr_attr with
      | Some (Constructor _) ->
         incr_k_ax_same_constructor cd ;
         (try f_implies_ax_constructor data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | Some (Initializer _) ->
         incr_k_ax_initializer cd ;
         (try f_implies_ax_initializer data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | Some (Projection  _) ->
         incr_k_ax_projection  cd ;
         (try f_implies_ax_projection data acc sign (qv_l, ax)
          with _ -> wrn_no_translation pos ; (acc, sign))
      | Some (Owise _) ->
         if is_predicate ax
         then (incr_k_ax_predicate_false cd ;
               (try f_implies_ax_predicate_false data acc sign (qv_l, ax)
                with _ -> wrn_no_translation pos ; (acc, sign)))
         else (incr_k_ax_owise    cd ;
               (try f_implies_ax_owise data acc sign (qv_l, ax)
                with _ -> wrn_no_translation pos ; (acc, sign)))
      | None ->
         if is_predicate ax
         then (incr_k_ax_predicate_true cd ;
               (try f_implies_ax_predicate_true data acc sign (qv_l, ax)
                with _ -> wrn_no_translation pos ; (acc, sign)))
         else (incr_k_ax_without_attr cd ;
               (try f_implies_ax_default data acc sign (qv_l, ax)
                with _ -> wrn_no_translation pos ; (acc, sign)))
      | _ ->
         if is_predicate ax
         then (incr_k_ax_predicate_true cd ;
               (try f_implies_ax_predicate_true data acc sign (qv_l, ax)
                with _ -> wrn_no_translation pos ; (acc, sign)))
         else (incr_k_ax_with_one_attr cd ;
               (try f_implies_ax_default data acc sign (qv_l, ax)
                with _ -> wrn_no_translation pos ; (acc, sign))))
        (* raise (InternalError "Need to update [axiom_cases], case Implies.")) *)
  | Rewrites _ -> (acc, sign) (* raise (InternalError "Rewriting translation not possible.") *)
    (* Ici, on pourrait s'attendre à renvoyer une erreur.
       C'est ce qu'il faudrait faire si [kommand_iter_with_alias] passait par là.
       Mais dans le cas de [kommand_iter_without_alias], cet axiome a déjà été traduit via [rewriting_cases].
       Nous préférons donc ne rien faire dans ce cas de figure. *)
  | _ -> raise (InternalError "Need to update [axiom_cases], case root.")

(** [rewriting_cases cd data curr_attr acc sign qv_l ax f_heating f_cooling f_semantic]
    acc ~ extra_data + sign ~ signature *)
let rewriting_cases
      (cd : count_data) (data : data) (acc : 'a) (sign : 's)
      (al : alias option) (qv_l : quant_var list) (ax : axiom)
      ((f_heating  : data -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)),
       (f_cooling  : data -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)),
       (f_semantic : data -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's))) : ('a * 's) =
  incr_k_rewriting_ax cd ;
  let pos = snd data in
  let al = match al with
    | None -> raise (InternalError "Need to update [rewriting_cases].")
    | Some al -> al
  in
  match data with
  | [Heat _], _ -> incr_k_ax_heating  cd ;
                   (try f_heating  data acc sign al (qv_l, ax)
                    with _ -> wrn_no_translation pos ; (acc, sign))
  | [Cool _], _ -> incr_k_ax_cooling  cd ;
                   (try f_cooling  data acc sign al (qv_l, ax)
                    with _ -> wrn_no_translation pos ; (acc, sign))
  | [Owise _], _ -> incr_k_ax_semantic cd ;
                    (try f_semantic data acc sign al (qv_l, ax)
                     with _ -> wrn_no_translation pos ; (acc, sign))
  | [Other (attr, _)], _ ->
     wrn_1 _STDOUT "New attribut (%s) used in a rewriting rule!" attr ; (acc, sign)
  (* | [Priority _] -> TODO update!
   wrn_1 _STDOUT ("Rules with priority isn't supported yet.") ; (acc, sign) *)
  (* raise (InternalError "The attribut priority isn't supported yet.") *)
  | [], _ -> incr_k_ax_semantic cd ;
             (try f_semantic data acc sign al (qv_l, ax)
              with _ -> wrn_no_translation pos ; (acc, sign))
  | _ -> raise (InternalError "Need to update [rewriting_cases].")

let meta_kommand_iter
      (meta_f_alias : kommand list -> data -> 'a -> 's -> alias -> ('a * 's))
      (meta_f_axiom : ('a, 's) meta_axiom -> ('a, 's) meta_axiom)
      (cd : count_data) (l : kommand list) (neutral_el : 'a) (init_sign : 's)
      (f_sort           : data -> 'a -> 's -> sort    -> ('a * 's))
      (f_hooked_sort    : data -> 'a -> 's -> sort    -> ('a * 's))
      (f_symbol         : data -> 'a -> 's -> symbol  -> ('a * 's))
      (f_hooked_symbol  : data -> 'a -> 's -> symbol  -> ('a * 's))
      (f_alias          : data -> 'a -> 's -> alias   -> ('a * 's))
      (f_ax_default     : ('a, 's) meta_axiom)
      ((f_exists_ax_subsort    : ('a, 's) meta_axiom),
       (f_exists_ax_functional : ('a, 's) meta_axiom)       as f_exists)
      ((f_equals_ax_assoc   : ('a, 's) meta_axiom),
       (f_equals_ax_comm    : ('a, 's) meta_axiom),
       (f_equals_ax_idem    : ('a, 's) meta_axiom),
       (f_equals_ax_unit    : ('a, 's) meta_axiom),
       (f_equals_ax_default : ('a, 's) meta_axiom)          as f_equals)
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
  let g_attr : ('a, 's) meta_axiom = fun ((_, pos) as data) acc sign (qv_l, ax) ->
    match data with
    | [], _ -> (if is_predicate ax
                then (incr_k_implies_ax cd ; incr_k_ax_predicate_true cd ;
                      (try f_implies_ax_predicate_true data acc sign (qv_l, ax) (* TODO Fix? *)
                       with _ -> wrn_no_translation (snd data) ; (acc, sign)))
             else (axiom_cases cd data None acc sign qv_l ax f_exists f_equals f_or_bottom f_not f_implies)) (* f_ax_default attr_l acc (qv_l, ax)) *)
    | [attr], _ -> axiom_cases cd data (Some attr) acc sign qv_l ax f_exists f_equals f_or_bottom f_not f_implies
    | _, _ ->
       (incr_k_ax_several_attr cd ;
        (* wrn_msg "There is an axiom with more than one attribute." ;
           @TODO print the list *)
        (try f_ax_default data acc sign (qv_l, ax)
         with _ -> wrn_no_translation pos ; (acc, sign)))
  in
  let rec aux l (acc, sign) = match l with
    | [] -> (acc, sign)
    | (c, (attr_l, pos))::q ->
       let res = match c with
         | Sort     s -> incr_k_sort cd        ;
                         (try f_sort (attr_l, pos) acc sign s
                          with _ -> wrn_no_translation pos ; (acc, sign))
         | H_sort   s -> incr_k_hooked_sort cd ;
                         (try f_hooked_sort (attr_l, pos) acc sign s
                          with _ -> wrn_no_translation pos ; (acc, sign))
         | Symbol   s -> incr_k_symbol cd        ;
                         (try f_symbol (attr_l, pos) acc sign s
                          with _ -> wrn_no_translation pos ; (acc, sign))
         | H_symbol s -> incr_k_hooked_symbol cd ;
                         (try f_hooked_symbol (attr_l, pos) acc sign s
                          with _ -> wrn_no_translation pos ; (acc, sign))
         | Alias   al -> meta_f_alias q (attr_l, pos) acc sign al
         | Axiom(qv_l, ax) -> incr_k_axiom cd ; meta_f_axiom g_attr (attr_l, pos) acc sign (qv_l, ax)
       in
       f_each_end_iter () ; aux q res
  in aux l (neutral_el, init_sign)

let kommand_iter_without_alias
      (cd : count_data) (l : kommand list) (neutral_el : 'a) (init_sign : 's)
      (f_sort           : data -> 'a -> 's -> sort    -> ('a * 's))
      (f_hooked_sort    : data -> 'a -> 's -> sort    -> ('a * 's))
      (f_symbol         : data -> 'a -> 's -> symbol  -> ('a * 's))
      (f_hooked_symbol  : data -> 'a -> 's -> symbol  -> ('a * 's))
      (f_alias          : data -> 'a -> 's -> alias   -> ('a * 's))
      ((f_rewrites_ax_heating  : data -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)),
       (f_rewrites_ax_cooling  : data -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)),
       (f_rewrites_ax_semantic : data -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)) as f_rewrites)
      (f_ax_default     : ('a, 's) meta_axiom)
      ((f_exists_ax_subsort    : ('a, 's) meta_axiom),
       (f_exists_ax_functional : ('a, 's) meta_axiom)       as f_exists)
      ((f_equals_ax_assoc   : ('a, 's) meta_axiom),
       (f_equals_ax_comm    : ('a, 's) meta_axiom),
       (f_equals_ax_idem    : ('a, 's) meta_axiom),
       (f_equals_ax_unit    : ('a, 's) meta_axiom),
       (f_equals_ax_default : ('a, 's) meta_axiom)          as f_equals)
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
  let meta_f_alias q data acc sign al = let pos = snd data in match q with
    | [] -> (incr_k_alias cd ;
             (try f_alias data acc sign al
              with _ -> wrn_no_translation pos ; (acc, sign)))
    | h::_ ->
       match h with
       | Axiom(qv_l, ax), (attr_l_ax, pos) ->
          let (attr_l, _) = data in
          let xdata = (attr_l@attr_l_ax, pos) in
          if is_rule ax
          then rewriting_cases cd xdata acc sign (Some al) qv_l ax f_rewrites
          else (incr_k_alias cd ;
                (try f_alias xdata acc sign al
                 with _ -> wrn_no_translation pos ; (acc, sign)))
       | _  -> (incr_k_alias cd ;
                (try f_alias data  acc sign al
                with _ -> wrn_no_translation pos ; (acc, sign)))
  in
  let meta_f_axiom g_attr data acc sign (qv_l, ax) =
    g_attr data acc sign (qv_l, ax)
  in
  meta_kommand_iter meta_f_alias meta_f_axiom cd l neutral_el init_sign f_sort f_hooked_sort
    f_symbol f_hooked_symbol f_alias f_ax_default
    f_exists f_equals f_or_bottom f_not f_implies f_each_end_iter

let kommand_iter_with_alias
      (cd : count_data) (l : kommand list) (neutral_el : 'a) (init_sign : 's)
      (f_sort           : data -> 'a -> 's -> sort    -> ('a * 's))
      (f_hooked_sort    : data -> 'a -> 's -> sort    -> ('a * 's))
      (f_symbol         : data -> 'a -> 's -> symbol  -> ('a * 's))
      (f_hooked_symbol  : data -> 'a -> 's -> symbol  -> ('a * 's))
      (f_alias          : data -> 'a -> 's -> alias   -> ('a * 's))
      ((f_rewrites_ax_heating  : data -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)),
       (f_rewrites_ax_cooling  : data -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)),
       (f_rewrites_ax_semantic : data -> 'a -> 's -> alias -> quant_var list * axiom -> ('a * 's)) as f_rewrites)
      (f_ax_default     : ('a, 's) meta_axiom)
      ((f_exists_ax_subsort    : ('a, 's) meta_axiom),
       (f_exists_ax_functional : ('a, 's) meta_axiom)       as f_exists)
      ((f_equals_ax_assoc   : ('a, 's) meta_axiom),
       (f_equals_ax_comm    : ('a, 's) meta_axiom),
       (f_equals_ax_idem    : ('a, 's) meta_axiom),
       (f_equals_ax_unit    : ('a, 's) meta_axiom),
       (f_equals_ax_default : ('a, 's) meta_axiom)          as f_equals)
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
  let meta_f_alias _ data acc sign al =
    incr_k_alias cd ;
    (try f_alias data acc sign al
     with _ -> wrn_no_translation (snd data) ; (acc, sign))
  in
  let meta_f_axiom g_attr data acc sign (qv_l, ax) =
    if is_rule ax
    then rewriting_cases cd data acc sign None qv_l ax f_rewrites
    else g_attr data acc sign (qv_l, ax)
  in
  meta_kommand_iter meta_f_alias meta_f_axiom cd l neutral_el init_sign f_sort f_hooked_sort
    f_symbol f_hooked_symbol f_alias f_ax_default
    f_exists f_equals f_or_bottom f_not f_implies f_each_end_iter
