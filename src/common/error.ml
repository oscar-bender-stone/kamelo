
open Color

(** Parameter for printing *)

let print     = Format.fprintf (* ppc = pretty printing channel *)
let _STDOUT   = Format.std_formatter

(* There is no dependent type in OCaml, so it seems that
   I need to duplicate the following function, one for each number
   of parameters. *)
let msg   ppc = fun msg -> print ppc msg ; print ppc "\n"
let msg_1 ppc = fun msg arg -> print ppc msg arg ; print ppc "\n"
let msg_2 ppc = fun msg arg1 arg2 ->
  print ppc msg arg1 arg2 ; print ppc "\n"

(** Coloried message *)

let wrn_msg ppc = fun msg -> print ppc (yel msg) ; print ppc "\n"
let wrn_1 ppc = fun msg arg -> print ppc (yel msg) arg ; print ppc "\n"
let wrn_2 ppc = fun msg arg1 arg2 ->
  print ppc (yel msg) arg1 arg2 ; print ppc "\n"
let wrn_3 ppc = fun msg arg1 arg2 arg3 ->
  print ppc (yel msg) arg1 arg2 arg3 ; print ppc "\n"

let blue_msg_1 ppc = fun msg arg -> print ppc (blu msg) arg ; print ppc "\n"

let red_msg   ppc = fun msg -> print ppc (red msg) ; print ppc "\n"
let red_msg_1 ppc = fun msg arg -> print ppc (red msg) arg ; print ppc "\n"
let red_msg_2 ppc = fun msg arg1 arg2 ->
  print ppc (red msg) arg1 arg2 ; print ppc "\n"
let red_msg_3 ppc = fun msg arg1 arg2 arg3 ->
  print ppc (red msg) arg1 arg2 arg3 ; print ppc "\n"

let cyan_msg_2 ppc = fun msg arg1 arg2 ->
  print ppc (cya msg) arg1 arg2 ; print ppc "\n"

let green_msg   ppc = fun msg -> print ppc (gre msg) ; print ppc "\n"
let green_msg_1 ppc = fun msg arg ->
  print ppc (gre msg) arg ; print ppc "\n"
let green_msg_2 ppc = fun msg arg1 arg2 ->
  print ppc (gre msg) arg1 arg2 ; print ppc "\n"

(** Exception *)

type kameloTypeError = InternalError | NotYetImplemented

(** A KaMeLo error has the following information:
      type error * file name * function name * specific message to specify the error *)
exception KaMeLoError of kameloTypeError * string * string * string

(** Message if translation fails *)

(** [wrn_no_translation ppc p1 p2] informs that the command, which begins line %i and
    ends line %i, wasn't translated, and the reason. *)
let wrn_no_translation = fun (typeError, fileName, functionName, msg) (p1, p2) ->
  match typeError with
  | InternalError ->
     red_msg_2 _STDOUT "ERROR:   The command, which begins line %i and ends line %i, wasn't translated:" p1 p2 ;
     red_msg_3 _STDOUT "\t The function [%s.%s] need to be fixed - %s\n" fileName functionName msg
  | NotYetImplemented ->
     wrn_2 _STDOUT "WARNING: The command, which begins line %i and ends line %i, wasn't translated." p1 p2 ;
     wrn_3 _STDOUT "\t The function [%s.%s] need to be updated - %s\n" fileName functionName msg
