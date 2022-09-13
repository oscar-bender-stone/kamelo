open Interface.LP_p_term

open LP.Syntax
open Mecanism.Count_data

let lib = ref false

(* TODO generalize the process *)
let rec printing_iter : (('a -> unit) * 'a list) list -> unit = function
  | []   -> ()
  | (prt, h)::q -> List.iter prt h ; printing_iter q

(* TODO delete *)
let prt_Viry ppc cd prt :
    p_command list * p_command list * p_symbol list * p_symbol list * p_rule list -> unit =
  fun (sort_l, sym_l, flat_sym_add_l, sym_add_l, r_l) ->
  List.iter (fun x -> incr_real_symbol cd ; prt ppc x) (List.rev sort_l) ;
  List.iter (fun x -> incr_real_symbol cd ; prt ppc x) (List.rev sym_l)  ;
  if (sym_add_l <> []) && not !lib then (* If there is at least one conditional rewriting rule *)
    (List.iter
       (fun x -> incr_additional_symbol cd ; prt ppc (create_LP_symbol x))
       (flat_sym_add_l)) ;
  List.iter
    (fun x -> incr_additional_symbol cd ; prt ppc (create_LP_symbol x))
    sym_add_l ;
  List.iter
    (fun x -> incr_real_rule cd ; prt ppc (create_multi_rule [x]))
    (List.rev r_l)
