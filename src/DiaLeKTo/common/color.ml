(** Format transformers (colors). *)
let colorize k ppc = "\027[" ^^ k ^^ "m" ^^ ppc ^^ "\027[0m%!"

(** Some colors *)
let red ppc = colorize "31" ppc
let gre ppc = colorize "32" ppc
let yel ppc = colorize "33" ppc
let blu ppc = colorize "34" ppc
let mag ppc = colorize "35" ppc
let cya ppc = colorize "36" ppc
