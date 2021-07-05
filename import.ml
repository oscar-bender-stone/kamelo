open Type
open Count_data
open Printer

let lp_pkg = "tests"
let prelude_path = ["Tests"] (* depuis lp_pkg *)
let prelude_name = "prelude"

let with_prelude : Format.formatter -> import list -> count_data -> unit =
  fun ppf i_l cd ->
  cd.k_import := List.length i_l;
  List.iter (pp_import ppf cd [lp_pkg]) (List.rev i_l);
  pp_import ppf cd (lp_pkg::prelude_path) (prelude_name, []);
