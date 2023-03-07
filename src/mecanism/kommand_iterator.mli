open Common.Type
open Count_data

type ('a, 's) meta_axiom = data -> 'a -> 's -> quant_var list * axiom -> ('a * 's)

(** [kommand_iter_without_alias
      cd l neutral_el init_sign
      f_sort f_hooked_sort f_symbol f_hooked_symbol f_alias
      (f_rewrites_ax_heating, f_rewrites_ax_cooling, f_rewrites_ax_semantic)     as f_rewrites
      f_ax_default
      (f_exists_ax_subsort, f_exists_ax_total)                                   as f_exists
      (f_equals_ax_assoc, f_equals_ax_comm,
       f_equals_ax_idem, f_equals_ax_unit, f_equals_ax_default)                  as f_equals
      f_or_bottom_ax_constructor                                                 as f_or_bottom
      f_not_ax_constructor                                                       as f_not
      (f_implies_ax_constructor, f_implies_ax_initializer,
       f_implies_ax_projection,  f_implies_ax_predicate_true,
       f_implies_ax_predicate_false, f_implies_ax_owise, f_implies_ax_default)   as f_implies
      f_claim
      f_each_end_iter]
    allows iteration on a Kore file, where the alias (LHS) and the RHS of rewriting rules are merged. *)
val kommand_iter_without_alias :
  count_data -> kommand list -> 'a -> 's
  -> (data -> 'a -> 's -> sort    -> 'a * 's)
  -> (data -> 'a -> 's -> sort    -> 'a * 's)
  -> (data -> 'a -> 's -> symbol  -> 'a * 's)
  -> (data -> 'a -> 's -> symbol  -> 'a * 's)
  -> (data -> 'a -> 's -> alias   -> 'a * 's)
  -> ((data -> 'a -> 's -> alias -> quant_var list * axiom -> 'a * 's) *
        (data -> 'a -> 's -> alias -> quant_var list * axiom -> 'a * 's) *
          (data -> 'a -> 's -> alias -> quant_var list * axiom -> 'a * 's))
  -> (('a, 's) meta_axiom)
  -> (('a, 's) meta_axiom * ('a, 's) meta_axiom)
  -> (('a, 's) meta_axiom * ('a, 's) meta_axiom
     * ('a, 's) meta_axiom * ('a, 's) meta_axiom * ('a, 's) meta_axiom)
  -> (('a, 's) meta_axiom)
  -> (('a, 's) meta_axiom)
  -> (('a, 's) meta_axiom * ('a, 's) meta_axiom
     * ('a, 's) meta_axiom * ('a, 's) meta_axiom
     * ('a, 's) meta_axiom * ('a, 's) meta_axiom * ('a, 's) meta_axiom)
  -> ('a, 's) meta_axiom
  -> (unit -> unit) -> ('a * 's)

(** [kommand_iter_with_alias] allows iteration on a Kore file, where the alias (LHS) and
    the RHS of rewriting rules are NOT merged.
    This function has the same signature that [kommand_iter_without_alias]. *)
val kommand_iter_with_alias :
  count_data -> kommand list -> 'a -> 's
  -> (data -> 'a -> 's -> sort    -> 'a * 's)
  -> (data -> 'a -> 's -> sort    -> 'a * 's)
  -> (data -> 'a -> 's -> symbol  -> 'a * 's)
  -> (data -> 'a -> 's -> symbol  -> 'a * 's)
  -> (data -> 'a -> 's -> alias   -> 'a * 's)
  -> ((data -> 'a -> 's -> alias -> quant_var list * axiom -> 'a * 's) *
        (data -> 'a -> 's -> alias -> quant_var list * axiom -> 'a * 's) *
          (data -> 'a -> 's -> alias -> quant_var list * axiom -> 'a * 's))
  -> (('a, 's) meta_axiom)
  -> (('a, 's) meta_axiom * ('a, 's) meta_axiom)
  -> (('a, 's) meta_axiom * ('a, 's) meta_axiom
     * ('a, 's) meta_axiom * ('a, 's) meta_axiom * ('a, 's) meta_axiom)
  -> (('a, 's) meta_axiom)
  -> (('a, 's) meta_axiom)
  -> (('a, 's) meta_axiom * ('a, 's) meta_axiom
     * ('a, 's) meta_axiom * ('a, 's) meta_axiom
     * ('a, 's) meta_axiom * ('a, 's) meta_axiom * ('a, 's) meta_axiom)
  -> ('a, 's) meta_axiom
  -> (unit -> unit) -> ('a * 's)
