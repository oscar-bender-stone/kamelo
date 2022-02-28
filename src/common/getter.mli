(** Getter for Kore command *)

open Type

(** For symbol *)
val has_no_param   : symbol -> bool
val get_name       : symbol -> name
val get_param      : symbol -> param
val get_sort       : symbol -> sort
val is_constructor : symbol -> attribute list -> sort option

(** For axiom *)
val is_predicate : axiom -> bool
val is_rule      : axiom -> bool
val is_conditional_rule : axiom -> bool
val is_cooling_rule : attribute list -> bool
val is_heating_rule : attribute list -> bool
