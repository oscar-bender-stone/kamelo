
open Common.Type
open Common.Xlib_OCaml
open LP.Syntax

open Interface.Signature

(** ****************************************************** *)
(** To translate exists-axioms (functional or subsort one) *)
(** ****************************************************** *)

val collect_subsort_data : axiom -> signature -> signature

val sym_case : name * param list * p_term list -> 's -> 'd -> p_term * 's * 'd
val var_case : (name -> p_term) -> name * param -> 's -> p_term StrMap.t -> p_term * 's * p_term StrMap.t

val curry_exec : (string -> p_term) -> axiom -> signature -> p_term * (string list) StrMap.t
val curry_exec_ident : axiom -> signature -> p_term * (string list) StrMap.t

val curry : (string -> p_term) -> axiom -> signature -> p_term StrMap.t -> p_term * p_term StrMap.t
val curry_ident   : axiom -> signature -> p_term StrMap.t -> p_term * p_term StrMap.t
val curry_pattern : axiom -> signature -> p_term StrMap.t -> p_term * p_term StrMap.t


(** **************************************************** *)
(** To translate equals-axioms
    (Associative, Commutative, Unit and Idempotence one) *)
(** **************************************************** *)

val of_equality_axiom : axiom -> p_rule

(** ****************************** *)
(** To translate implies-axioms    *)
(** ****************************** *)

(** Type of extra data about a rule *) (* TODO add priority ? *)
type extra_data_rule =
 | Uncond         (* A uncondtional rule *)
 | Cond of p_term (* A conditional rule with a condition *)
 | OwiseRule      (* A rule with the attribut "owise" *)

(** Type of a rule in a CTRS, which has the form
    ((LHS, RHS), extra_data_rule, priority) *)
type ctrs_rule = p_rule * extra_data_rule * int

(** [of_implies_axiom ax] translates the axiom [ax] which begins by
    "\implies" to a rewriting rule. *)
val of_implies_axiom : axiom -> ctrs_rule
