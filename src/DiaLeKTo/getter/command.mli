open Type.Term
open Type.Command

(** ********************* *)
(**  Logic statement      *)
(** ********************* *)

(** ***************** *)
(** A. Symbol         *)
(** ***************** *)

(** About the parsing of the symbol *)
val get_sym_assoc        : sym_decl -> associativity option
val get_sym_prec         : sym_decl -> precedence
val get_sym_mixfix       : sym_decl -> mixfix
val get_sym_parsing_rule : sym_decl -> parsing_rule

(** About the visibility of the symbol *)
val get_sym_visibility : sym_decl -> visibility

(** About the property of the symbol *)
val get_sym_algebra  : sym_decl -> algebra
val get_sym_property : sym_decl -> property
val get_sym_name     : sym_decl -> iden

(** About the type of the symbol *)
val get_sym_implicit_var : sym_decl -> param list
val get_sym_explicit_var : sym_decl -> param list
val split_sym_var        : sym_decl -> param list * param list
val get_sym_type         : sym_decl -> term

(** ***************** *)
(** B. Definition     *)
(** ***************** *)

(** About the parsing of the definition *)
val get_def_assoc        : def_decl -> associativity option
val get_def_prec         : def_decl -> precedence
val get_def_mixfix       : def_decl -> mixfix
val get_def_parsing_rule : def_decl -> parsing_rule

(** About the visibility of the definition *)
val get_def_visibility : def_decl -> visibility

(** About the property of the definition *)
val get_def_algebra  : def_decl -> algebra
val get_def_property : def_decl -> property
val get_def_name     : def_decl -> iden

(** About the type of the definition *)
val get_def_implicit_var : def_decl -> param list
val get_def_explicit_var : def_decl -> param list
val split_def_var        : def_decl -> param list * param list
val get_def_type         : def_decl -> term option

(** About the body of the definition *)
val get_lambda_term : def_decl -> term option
val is_opaque       : def_decl -> bool

(** ***************** *)
(** C. Rewriting rule *)
(** ***************** *)

(* val get_rule_var : rule -> iden list * int * iden list *)

(** ********************* *)
(**  Set option           *)
(** ********************* *)

(* TODO *)

(** ********************* *)
(**  Query                *)
(** ********************* *)

(* TODO *)

(** ********************* *)
(**  (Un)safe command     *)
(** ********************* *)

(* TODO *)
