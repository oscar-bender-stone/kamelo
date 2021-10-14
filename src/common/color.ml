let no_color = ref false

(** Format transformers (colors). *)
let colorize k fmt =
  if !no_color then fmt else "\027[" ^^ k ^^ "m" ^^ fmt ^^ "\027[0m%!"

(** Some colors *)
let red fmt = colorize "31" fmt
let gre fmt = colorize "32" fmt
let yel fmt = colorize "33" fmt
let blu fmt = colorize "34" fmt
let mag fmt = colorize "35" fmt
let cya fmt = colorize "36" fmt


type output  = Format.formatter
