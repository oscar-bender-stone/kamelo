
open LP_p_term
open Output

let _SORTK = pp "SortK"
let _INJD  = pp "δ"
let _INJ   = pp "inj"
let _KSEQ  = pp "kseq"
let _DOTK  = pp "dotk"

let _TRUE  = pp "true"
let _FALSE = pp "false"

let p_SORTK = create_ident _SORTK
let p_INJD  = create_ident _INJD
let p_INJ   = create_ident _INJ
let p_KSEQ  = create_ident _KSEQ
let p_DOTK  = create_ident _DOTK

let p_TRUE  = create_ident _TRUE
let p_FALSE = create_ident _FALSE


let _AND_BOOL = pp "Lbl'Unds'andBool'Unds"
let _OR_BOOL  = pp "Lbl'Unds'orBool'Unds"
let _NOT_BOOL = pp "LblnotBool'Unds"
(* let _EQUALS_BOOL = "\equals"
let _DOMAIN_VALUES = *)

(** The cell's name of the cell k *)
let _K_CELL = pp "Lbl'-LT-'k'-GT-'" (* "<k>" *)
