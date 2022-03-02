
open Common.Type
open Common.Error

(** [axiom_iter qv_l ax f_var init_sign init_local_data
    f_predicate_sym f_predicate_var f_dom_val
    f_bottom f_top f_in f_exists f_not f_equals f_or f_and f_implies f_rewrites]
    to iterate over an axiom [ax]. *)
let axiom_iter
  (qv_l : quant_var list) (ax : axiom)
  (f_var : string -> 'r) (init_sign : 's) (init_local_data : 'd)
  (f_predicate_sym : name * param list * 'r list -> 's -> 'd -> 'r * 's * 'd) (* 'r or 'r list ? *)
  (f_predicate_var : name * param                -> 's -> 'd -> 'r * 's * 'd)
  (f_dom_val  : sort * name                      -> 's -> 'd -> 'r * 's * 'd)
  (f_bottom   : param list                       -> 's -> 'd -> 'r * 's * 'd)
  (f_top      : param list                       -> 's -> 'd -> 'r * 's * 'd)
  (f_in       : param list * (name * param) * 'r -> 's -> 'd -> 'r * 's * 'd)
  (f_exists   : param list * (name * param) * 'r -> 's -> 'd -> 'r * 's * 'd)
  (f_not      : param list * 'r                  -> 's -> 'd -> 'r * 's * 'd)
  (f_equals   : param list * 'r * 'r             -> 's -> 'd -> 'r * 's * 'd)
  (f_or       : param list * 'r * 'r             -> 's -> 'd -> 'r * 's * 'd)
  (f_and      : param list * 'r * 'r             -> 's -> 'd -> 'r * 's * 'd)
  (f_implies  : param list * 'r * 'r             -> 's -> 'd -> 'r * 's * 'd)
  (f_rewrites : param list * 'r * 'r             -> 's -> 'd -> 'r * 's * 'd) : 'r * 's * 'd =
  let rec aux : axiom -> 's -> 'd -> 'r * 's * 'd = fun ax sign local_data ->
    match ax with
    | Predicate(Sym(n, qv_l, a_l)) ->    (* ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
       let f : ('r list * 's * 'd) -> axiom -> ('r list * 's * 'd) =
         fun (acc, s, d) ax -> let r, s, d = aux ax s d in (r::acc, s, d)
       in
       let r, s, d = List.fold_left f ([], sign, local_data) a_l in
       f_predicate_sym (n, qv_l, r) s d
    | Predicate(Var(n, p))         -> f_predicate_var (n, p) sign local_data
    | Dom_val(s, name)             -> f_dom_val    (s, name) sign local_data
    | Bottom p_l                   -> f_bottom     p_l       sign local_data
    | Top p_l                      -> f_top        p_l       sign local_data
    | In(p_l, (v,p), ax)           ->
       let r, s, d = aux ax sign local_data in f_in     (p_l, (v,p), r) s d
    | Exists(p_l, (v,p), ax)       ->
       let r, s, d = aux ax sign local_data in f_exists (p_l, (v,p), r) s d
    | Not(p_l, ax)                 ->
       let r, s, d = aux ax sign local_data in f_not    (p_l, r) s d
    | Equals(p_l, ax1, ax2)        ->
       let r1, s1, d1 = aux ax1 sign local_data in
       let r2, s2, d2 = aux ax2 s1 d1           in f_equals   (p_l, r1, r2) s2 d2
    | And(p_l, ax1, ax2)           ->
       let r1, s1, d1 = aux ax1 sign local_data in
       let r2, s2, d2 = aux ax2 s1 d1           in f_and      (p_l, r1, r2) s2 d2
    | Or(p_l, ax1, ax2)            ->
       let r1, s1, d1 = aux ax1 sign local_data in
       let r2, s2, d2 = aux ax2 s1 d1           in f_or       (p_l, r1, r2) s2 d2
    | Implies(p_l, ax1, ax2)       ->
       let r1, s1, d1 = aux ax1 sign local_data in
       let r2, s2, d2 = aux ax2 s1 d1           in f_implies  (p_l, r1, r2) s2 d2
    | Rewrites(p_l, ax1, ax2)      ->
       let r1, s1, d1 = aux ax1 sign local_data in
       let r2, s2, d2 = aux ax2 s1 d1           in f_rewrites (p_l, r1, r2) s2 d2
  in
  aux ax init_sign init_local_data

(** [axiom_iter_default_error] is similar to [axiom_iter] with default errors. *)
let axiom_iter_default_error
  (qv_l : quant_var list) (ax : axiom)
  (f_var : string -> 'r) (init_sign : 's) (init_local_data : 'd)
  (f_predicate_sym : name * param list * 'r list -> 's -> 'd -> 'r * 's * 'd) (* 'r or 'r list ? *)
  (f_predicate_var : name * param                -> 's -> 'd -> 'r * 's * 'd)
  (f_dom_val  : sort * name                      -> 's -> 'd -> 'r * 's * 'd)
  (f_not      : param list * 'r                  -> 's -> 'd -> 'r * 's * 'd)
  (f_equals   : param list * 'r * 'r             -> 's -> 'd -> 'r * 's * 'd)
  (f_and      : param list * 'r * 'r             -> 's -> 'd -> 'r * 's * 'd) : 'r * 's * 'd =
  let f_bottom_err _ _ _ =
      raise (NotYetImplemented "Need to update [axiom_iter_default_error] - Case bottom") in
  let f_top_err _ _ _ =
      raise (NotYetImplemented "Need to update [axiom_iter_default_error] - Case top")    in
  let f_in_err _ _ _ =
      raise (NotYetImplemented "Need to update [axiom_iter_default_error] - Case in")     in
  let f_exists_err _ _ _ =
      raise (NotYetImplemented "Need to update [axiom_iter_default_error] - Case exists") in
  let f_or_err _ _ _ =
      raise (NotYetImplemented "Need to update [axiom_iter_default_error] - Case or")       in
  let f_implies_err _ _ _ =
      raise (NotYetImplemented "Need to update [axiom_iter_default_error] - Case implies")  in
  let f_rewrites_err _ _ _ =
      raise (NotYetImplemented "Need to update [axiom_iter_default_error] - Case rewrites") in
  axiom_iter qv_l ax f_var init_sign init_local_data
    f_predicate_sym f_predicate_var f_dom_val
    f_bottom_err f_top_err f_in_err f_exists_err
    f_not f_equals
    f_or_err
    f_and
    f_implies_err f_rewrites_err
