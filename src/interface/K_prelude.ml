
open LP_p_term

let _SORTK = "SortK"
let _SORT_KITEM = "SortKItem"
let _SORT_KRESULT = "SortKResult"
let _SORT_ID = "SortId"
let _INJD  = "δ"
let _INJ   = "inj"
let _KSEQ  = "kseq"
let _DOTK  = "dotk"

let _TRUE  = "true"
let _FALSE = "false"

let p_SORTK = create_ident _SORTK
let p_SORT_KITEM = create_ident _SORT_KITEM
let p_SORT_KRESULT = create_ident _SORT_KRESULT
let p_INJD  = create_ident _INJD
let p_INJ   = create_ident _INJ
let p_KSEQ  = create_ident _KSEQ
let p_DOTK  = create_ident _DOTK

let p_TRUE  = create_ident _TRUE
let p_FALSE = create_ident _FALSE


let _AND_BOOL = "Lbl'Unds'andBool'Unds'"
let _OR_BOOL  = "Lbl'Unds'orBool'Unds'"
let _NOT_BOOL = "LblnotBool'Unds'"
(* let _EQUALS_BOOL = "\equals"
let _DOMAIN_VALUES = *)
let p_AND_BOOL = create_ident _AND_BOOL
let p_OR_BOOL  = create_ident _OR_BOOL
let p_NOT_BOOL = create_ident _NOT_BOOL

let _EQ_K  = "Lbl'UndsEqlsEqls'K'Unds'"
let p_EQ_K = create_ident _EQ_K

(** The cell's name of the cell k *)
let _K_CELL  = "Lbl'-LT-'k'-GT-'" (* "<k>" *)
let p_K_CELL = create_ident _K_CELL

(** The specific predicate about the resulting values *)
let _IS_KRESULT  = "isKResult"
let p_IS_KRESULT = create_ident _IS_KRESULT
