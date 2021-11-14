
open Common.Type
open LP.Syntax

type t = axiom

exception KComputation of string
exception ConditionalRule of string

module StrMap : Map.S with type key = string

val free_var : (string list) StrMap.t ref

val data_matching : p_term StrMap.t ref

val curry : (string -> p_term) -> t -> p_term
val curry_ident   : t -> p_term
val curry_pattern : t -> p_term

val of_equality_axiom : t -> p_rule

(* val subsort_data : (string list) StrMap.t ref *)
val from_subsort_axiom : string -> string -> unit

(** Type of extra data about a rule *) (* Mettre aussi priority ? *)
type extra_data_rule =
   Uncond         (* A uncondtional rule *)
 | Cond of p_term (* A conditional rule with a condition *)
 | OwiseRule      (* A rule with the attribut "owise" *)

(** Type of a rule in a CTRS, which has the form
    ((LHS, RHS), extra_data_rule, priority) *)
type ctrs_rule = p_rule * extra_data_rule * int

(** [of_implies_axiom ax] translates the axiom [ax] which begins by "\implies"
    to a rewriting rule. *)
val of_implies_axiom : t -> ctrs_rule

val is_predicate : t -> bool
val is_rule      : t -> bool
val is_conditional_rule : t -> bool

(** [create_rewriting_rule al ax] creates a rewriting rule thanks to
    an alias (for LHS) and an axiom (for RHS). *)
val create_rewriting_rule : alias -> t -> p_rule

val sort_signature : p_term StrMap.t ref
val create_isKResult_rule : unit -> p_rule list
