(** To know the number of the current line, when parsing. *)
let curr_line  = ref 1

(** To know the number of the starting line of a command, when parsing. *)
let start_line = ref 1

(** [update_line ()] returns the couple (bline, lline) where
      bline is the first line of the command
      lline is the last one. *)
let update_line : unit -> int * int = fun () ->
  let res = (!start_line, !curr_line - 1) in
  start_line := !curr_line ; res
