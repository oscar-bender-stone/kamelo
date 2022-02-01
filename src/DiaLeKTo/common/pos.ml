(** Source code position management.  This module may be used to map sequences
    of characters in a source file to an abstract syntax tree. *)


(** Short name for a standard formatter with continuation. *)
type ('a,'b) koutfmt = ('a, Format.formatter, unit, unit, unit, 'b) format6

