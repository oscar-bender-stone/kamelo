
open Common.Type
open Common.Xlib_OCaml
open LP.Syntax

open Interface.Signature

type t = axiom

(** ****************************************************** *)
(** To translate exists-axioms (functional or subsort one) *)
(** ****************************************************** *)

val free_var : (string list) StrMap.t ref

val data_matching : p_term StrMap.t ref

val do_specific_thing : bool ref
val reset_var : unit -> unit

val change_sort_inj : p_term -> signature -> p_term

val collect_subsort_data : axiom -> signature -> signature


val curry : (string -> p_term) -> t -> signature -> p_term
val curry_ident   : t -> signature -> p_term
val curry_pattern : t -> signature -> p_term


(** **************************************************** *)
(** To translate equals-axioms
    (Associative, Commutative, Unit and Idempotence one) *)
(** **************************************************** *)

val of_equality_axiom : t -> p_rule

(** ****************************** *)
(** To translate implies-axioms    *)
(** ****************************** *)

(** Type of extra data about a rule *) (* Mettre aussi priority ? *)
type extra_data_rule =
 | Uncond         (* A uncondtional rule *)
 | Cond of p_term (* A conditional rule with a condition *)
 | OwiseRule      (* A rule with the attribut "owise" *)

(** Type of a rule in a CTRS, which has the form
    ((LHS, RHS), extra_data_rule, priority) *)
type ctrs_rule = p_rule * extra_data_rule * int

(** [of_implies_axiom ax] translates the axiom [ax] which begins by "\implies"
    to a rewriting rule. *)
val of_implies_axiom : t -> ctrs_rule
