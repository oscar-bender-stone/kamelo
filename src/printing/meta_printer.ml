open Interface.LP_p_term

open LP.Syntax
open Mecanism.Count_data

(* TODO generalize the process *)
let rec printing_iter : (('a -> unit) * 'a list) list -> unit = function
  | []   -> ()
  | (prt, h)::q -> List.iter prt h ; printing_iter q

(* TODO delete *)
let prt_Viry ppc cd prt :
    p_command list * p_command list * p_symbol list * p_rule list -> unit =
  fun (sort_l, sym_l, sym_add_l, r_l) ->
  List.iter (fun x -> incr_real_symbol cd ; prt ppc x) (List.rev sort_l) ;
  List.iter (fun x -> incr_real_symbol cd ; prt ppc x) (List.rev sym_l)  ;
  if List.length sym_add_l > 3 then (* If there is at least one conditional rewriting rule *)
    (List.iter
       (fun x -> incr_additional_symbol cd ; prt ppc (create_LP_symbol x))
       (List.rev sym_add_l)) ;
  List.iter
    (fun x -> incr_real_rule cd ; prt ppc (create_multi_rule [x]))
    (List.rev r_l)
