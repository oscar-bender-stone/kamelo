
(** Term for type *)

type iden = string

(* VERSION SIMPL

(* TODO remove - EST-CE QU'IL FAUT FAIRE UNE DIFFERENCE ??? *)
type var = string
type iden = string

type term =
  | Type
  | Var of var
  | Sym of iden
  | Appl of term * term
  | Lambda of var * term option * term
  | Pi of var * term option * term
          *)

type term =
  | Type
  | Sym of iden
  | Var of iden
  | Appl of term * term list
  | Lambda of (iden list * term option) * term
  | Pi of (iden list * term option) * term
  | Arrow of term * term

(* Impl ? *)

(* Meta-variable *)
(* Placeholder *)
(* TRef ? *)
(* TEnv ? *)
(* Let *)

(** Term for rewriting rule *)

type pattern =
  | Wildcard
  | Var of iden
  | Pattern of iden * pattern list  (** Applied constant    *)

type lhs = iden * pattern list

  (*
type pattern =
  (* | Var of iden * int * pattern list  (** Applied DB variable *) TODO *)
  | Wildcard
  | Var of var
  | Pattern of iden * pattern list  (** Applied constant    *)
  (* | Lambda of loc * ident * pattern  (** Lambda abstraction  *) TODO
     | Brackets of term  (** Bracket of a term   *) TODO *)
   *)
