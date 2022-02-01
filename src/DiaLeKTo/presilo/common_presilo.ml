open Dcommon.Error

let format_of_sep str ppc () : unit = print ppc "%s" str

let pp_list sep pp ppc l =
  Format.pp_print_list ~pp_sep:(format_of_sep sep) pp ppc l
