
open Format
open Color

let print = fprintf (* ppc = pretty printing channel *)

let wrn_msg ppc = fun msg -> print ppc (yel msg) ; print ppc "\n"

let wrn_tool = fun ppc feature tool changement ->
  let msg =
    "**WARNING**: The " ^^ feature ^^ " isn't possible in " ^^ tool ^^ ".\n \
     This was translated into " ^^ changement ^^ "."
  in
  wrn_msg ppc msg

let wrn_dk ppc feature changement =
  wrn_tool ppc feature "Dedukti"  changement
let wrn_lp ppc feature changement =
  wrn_tool ppc feature "Lambdapi" changement

(* let t = wrn_lp Format.std_formatter "Protected" "Public";;
   let t = wrn_msg Format.std_formatter "Hello" ;; *)

(* POSSIBLE ?
let wrn_msg = fun (ppc : formatter) msg arg_l ->
  match arg_l with
  | []   -> print ppc (yel ppc msg)
  | t::q -> List.fold_left (fun a b -> print ppc (yel ppc msg) a b) t q

(*  List.fold_left  print ppc (yel "W %s %s") "h" "h"
    print (yel "WARNING: %s has been restored.\n") n *)
;;
let t = wrn_msg Format.std_formatter ("WARNIG: %s" "hello")

 *)

(* IDEA
exception NoProofInDedukti of string
exception BadCommand

let warning_ : -> -> -> = fun cfg f t ->
  (match cfg with
   | None, None -> ()
   | _,_ -> print "WARNING: Not possible un LP") ;
  f t
 *)
