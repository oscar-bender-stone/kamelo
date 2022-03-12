open Common.Xlib_OCaml
open Common.Type
open LP.Syntax
open Interface.Signature

val curry_exec_ident : axiom -> signature -> p_term * (string list) StrMap.t
