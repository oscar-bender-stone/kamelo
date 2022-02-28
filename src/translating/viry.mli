
open Common.Xlib_OCaml
open LP.Syntax
open Axiom

val safe_prefix : string

(* val symb_signature : p_term StrMap.t ref *)

val viry_encoding :
  ctrs_rule list -> p_term StrMap.t -> p_symbol list * p_rule list
