
let readable = ref false

(** [pretty_string iso s] replaces each pattern in the string [s] according to
    the list of string pairs [iso]. *)
let pretty_string : (string * string) list -> string -> string = fun iso s ->
  let rec aux l s = match l with
     | [] -> s
     | (pattern, new_s)::t ->
        aux t (Str.global_replace (Str.regexp pattern) new_s s)
  in
  aux iso s

(** [skip_sign s] creates a pattern with the string [s] to delete
    the signature encoding in the name of a symbol. *)
let skip_sign s = "_\\([A-Z-]*\\)" ^ s ^ "\\([A-Za-z_-]+\\)"

(** [string_symbol_isomorphism] is a list of string pairs.
    This list is useful to replace the first component of a pair by the second one. *)
let string_symbol_isomorphism =
  [ ("Lbl", "") ; ("Var", "") ; (* ("Sort", "") ; ("Stop", ".") ; *) ("Unds", "_") ; ("'", "") ;  ("-LT-", "<") ; ("-GT-", ">") ;
    ("Pipe", "|") ; ("Eqls", "=") ; ("Slsh", "/") ; ("Hash", "#") ; ("Tild", "~") ; ("Perc", "%") ; ("Star", "*") ; ("Quot", "") ;
    ("projectColn", "proj_") (*; ("project", "π") ; ("Plus", "+")
    ; ("LPar", "(") ; ("RPar", ")") ; ("LSqB", "[") ; ("RSqB", "]") ; ("LBra", "{") ; ("RBra", "}") ;
    ("Comm", ",") ; ("Coln", ":") ; ("SCln", ";") *) ; ("LPar_\\([Comm_]*\\)RPar", "")
    ; (skip_sign "-SYNTAX", "_") ; (skip_sign "-COMMON", "")
    ; (skip_sign "INT", "_INT") ; (skip_sign "LIST", "_LIST") ; (skip_sign "SET", "_SET") ; (skip_sign "MAP", "_MAP") ]

(** To make readable symbol name *)
let pp s = if !readable then pretty_string string_symbol_isomorphism s else s
