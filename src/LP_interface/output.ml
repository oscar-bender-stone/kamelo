
type mimic_management = K | Kore | Dedukti
let mimic = ref Kore

type output_management = LP | Dedukti | Kore
let output = ref LP

let create_filename name =
  let tmp = String.lowercase_ascii name in
  match !output with
  | Dedukti -> tmp ^ ".dk"
  | LP      -> tmp ^ ".lp"
  | Kore    -> tmp ^ ".mykore"

let readable = ref false

(* Meilleure complexité avec une map, mais moins lisible *)

(*
let pretty_name : string -> (string * string) list -> string = fun s iso ->
  let len = ref (String.length s) in
  let res = ref s in
  let rec aux l = match l with
     | [] -> !res
     | (pattern, new_s)::t ->
        let head_len = String.length pattern in
        let new_len  = String.length new_s   in
        let rec comparison k i pattern j =
          if j = head_len then
            (res := String.sub !res 0 k ^ new_s ^ String.sub !res (k+new_len+1) (!len-(k+new_len+1)-1);
             len := String.length !res)
          else
            if i+k < !len && j < head_len && !res.[i] = pattern.[j]
            then comparison  k    (i+1) pattern (j+1)
            else comparison (k+1) (k+1) pattern  0
        in
        comparison 0 0 pattern 0;
        (*for k = 0 to !len-1 do
          res := comparison k !res k pattern 0;
          len := String.length !res
        done; *)
        aux t
  in
  aux iso
 *)

let pretty_string : (string * string) list -> string -> string = fun iso s ->
  let rec aux l s = match l with
     | [] -> s
     | (pattern, new_s)::t ->
        aux t (Str.global_replace (Str.regexp pattern) new_s s)
  in
  aux iso s

let skip_sign s = "_\\([A-Z-]*\\)" ^ s ^ "\\([A-Za-z_-]+\\)"

let string_symbol_isomorphism =
  [ ("Lbl", "") ; ("Var", "") ; (* ("Sort", "") ; ("Stop", ".") ; *) ("Unds", "_") ; ("'", "") ;  ("-LT-", "<") ; ("-GT-", ">") ;
    ("Pipe", "|") ; ("Eqls", "=") ; ("Slsh", "/") ; ("Hash", "#") ; ("Tild", "~") ; ("Perc", "%") ; ("Star", "*") ; ("Quot", "'") ;
    ("projectColn", "proj_") (*; ("project", "π") ; ("Plus", "+")
    ; ("LPar", "(") ; ("RPar", ")") ; ("LSqB", "[") ; ("RSqB", "]") ; ("LBra", "{") ; ("RBra", "}") ;
    ("Comm", ",") ; ("Coln", ":") ; ("SCln", ";") *) ; ("LPar_\\([Comm_]*\\)RPar", "")
    ; (skip_sign "-SYNTAX", "_") ; (skip_sign "-COMMON", "")
    ; (skip_sign "INT", "_INT") ; (skip_sign "LIST", "_LIST") ; (skip_sign "SET", "_SET") ; (skip_sign "MAP", "_MAP") ]

let pp s = if !readable then pretty_string string_symbol_isomorphism s else s
