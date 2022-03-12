open Common.Xlib_OCaml
open Common.Type
open LP.Syntax
open Interface.Signature

val iter_exec : axiom -> signature -> p_term * (string list) StrMap.t
