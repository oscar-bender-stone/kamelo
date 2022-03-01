
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

let blue_msg_1 ppc = fun msg arg -> print ppc (blu msg) arg ; print ppc "\n"

let red_msg   ppc = fun msg -> print ppc (red msg) ; print ppc "\n"
let red_msg_1 ppc = fun msg arg -> print ppc (red msg) arg ; print ppc "\n"

let cyan_msg_2 ppc = fun msg arg1 arg2 ->
  print ppc (cya msg) arg1 arg2 ; print ppc "\n"

let green_msg   ppc = fun msg -> print ppc (gre msg) ; print ppc "\n"
let green_msg_1 ppc = fun msg arg ->
  print ppc (gre msg) arg ; print ppc "\n"
let green_msg_2 ppc = fun msg arg1 arg2 ->
  print ppc (gre msg) arg1 arg2 ; print ppc "\n"

(** Exception *)

exception InternalError of string
exception NotYetImplemented of string
