open Common.Type
open Common.Count_data
open Printer

let lp_pkg = "root_KaMeLo"
let prelude_path = ["tests"] (* depuis lp_pkg *)
let prelude_name = "prelude"

let all_import : (import list) ref = ref []

let rec concat_no_duplicate l1 l2 = match l1 with
  | [] -> l2
  | t::q -> if List.mem t l2
            then concat_no_duplicate q l2
            else concat_no_duplicate q (t::l2)

let with_prelude : output -> printer -> import list -> count_data -> unit =
  fun ppf prt i_l cd ->
  cd.k_import := List.length i_l;
  all_import  := concat_no_duplicate i_l !all_import;
  List.iter (pp_import ppf cd prt [lp_pkg]) !all_import;
  pp_import ppf cd prt (lp_pkg::prelude_path) (prelude_name, []);
