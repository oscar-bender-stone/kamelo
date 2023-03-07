
type count_data

(** [reset_count_data i] returns a value of type "count_data"
    where all internal values are initialised at [i]. *)
val reset_count_data : int -> count_data

(** Data about the K commands *)

val set_k_import : count_data -> int -> unit

val incr_k_import          : count_data -> unit
val incr_k_sort            : count_data -> unit
val incr_k_hooked_sort     : count_data -> unit
val incr_k_symbol          : count_data -> unit
val incr_k_hooked_symbol   : count_data -> unit
val incr_k_alias           : count_data -> unit
val incr_k_axiom           : count_data -> unit
val incr_k_ax_without_attr : count_data -> unit
val incr_k_ax_several_attr : count_data -> unit
val incr_k_claim           : count_data -> unit

(** Data about the K axioms *)

(* Exists one *)
val incr_k_exists_ax     : count_data -> unit
val incr_k_ax_subsort    : count_data -> unit
val incr_k_ax_functional : count_data -> unit

(* Equals one *)
val incr_k_equals_ax : count_data -> unit
val incr_k_ax_assoc  : count_data -> unit
val incr_k_ax_comm   : count_data -> unit
val incr_k_ax_idem   : count_data -> unit
val incr_k_ax_unit   : count_data -> unit

(* Or one *)
val incr_k_or_ax                  : count_data -> unit
val incr_k_or_ax_junk_constructor : count_data -> unit

(* Bottom one *)
val incr_k_bottom_ax                  : count_data -> unit
val incr_k_bottom_ax_junk_constructor : count_data -> unit

(* Not one *)
val incr_k_not_ax                  : count_data -> unit
val incr_k_not_ax_diff_constructor : count_data -> unit

(* Implies one *)
val incr_k_implies_ax          : count_data -> unit
val incr_k_ax_same_constructor : count_data -> unit
val incr_k_ax_initializer      : count_data -> unit
val incr_k_ax_projection       : count_data -> unit
val incr_k_ax_predicate_false  : count_data -> unit
val incr_k_ax_predicate_true   : count_data -> unit
val incr_k_ax_owise            : count_data -> unit
val incr_k_ax_with_one_attr    : count_data -> unit

(* Rewriting one *)
val incr_k_rewriting_ax : count_data -> unit
val incr_k_ax_heating   : count_data -> unit
val incr_k_ax_cooling   : count_data -> unit
val incr_k_ax_semantic  : count_data -> unit

(** Data about the Dedukti commands *)

val incr_real_import       : count_data -> unit
val incr_real_symbol       : count_data -> unit
val incr_additional_symbol : count_data -> unit
val incr_real_induc        : count_data -> unit
val incr_real_rule         : count_data -> unit

(** To print count data *)
val extract_info_before : count_data -> (int * int * string * string) list
val extract_info_after  : count_data -> (int * int * string * string) list
