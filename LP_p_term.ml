
open Syntax

open Display_console

let _SORTK = "SortK"
let _INJ = "injK"
let _INJGEN = "injG"
let _TOPCELL = "GeneratedTopCell"


let create_ident : string -> p_term = fun s ->
  Pos.none (P_Iden(Pos.none ([], (pp s)), false))

let create_meta_var : string -> p_meta_ident = fun s ->
  Pos.none (Name s)

let create_pattern_var : string -> p_term = fun s ->
  Pos.none (P_Patt(Some (Pos.none (pp s)), None))

let create_explicit_arg : string -> p_term = fun s ->
  Pos.none (P_Expl(create_ident s))

let get_type : string -> p_term = fun s ->
  if s = _SORTK then create_ident s
  else Pos.none (P_Appl(create_ident _INJ, create_ident s))
