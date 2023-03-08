open LP.Syntax
open Interface.Signature
open Axiom

val safe_prefix : string

val viry_encoding :
  ctrs_rule list -> signature -> p_symbol list * (p_symbol list * p_rule list)
