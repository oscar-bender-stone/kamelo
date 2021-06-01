
open Syntax


let create_meta_var : string -> p_meta_ident = fun s ->
  Pos.none (Name s)

let create_ident : string -> p_term = fun s ->
  Pos.none (P_Iden(Pos.none ([], s), false))

let create_pattern_var : string -> p_term = fun s ->
  Pos.none (P_Patt(Some (Pos.none s), None))
