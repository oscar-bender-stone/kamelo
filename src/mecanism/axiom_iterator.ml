
open Common.Type

(** [axiom_cases cd attr_l curr_attr acc sign qv_l ax f_exists f_equals f_or_bottom f_not f_implies]
    acc ~ extra_data + sign ~ signature *)
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
