
open Color

type count_data = { k_import : int ref        ; real_import : int ref
                  ; k_sort   : int ref
                  ; k_hooked_sort : int ref   ; real_sort   : int ref
                  ; k_symbol : int ref
                  ; k_hooked_symbol : int ref ; real_symbol : int ref
                  ; real_induc : int ref
                  ; k_alias : int ref         ; real_alias  : int ref
                  ; k_rule  : int ref         ; real_rule   : int ref
                  ; k_axiom : int ref         ; real_axiom  : int ref }

let get_k_import cd = !(cd.k_import)
let set_k_import cd i = cd.k_import := i
let incr_k_import cd = incr cd.k_import

let get_real_import cd = !(cd.real_import)
let incr_real_import cd = incr cd.real_import

let get_k_sort cd = !(cd.k_sort)
let incr_k_sort cd = incr cd.k_sort

let get_k_hooked_sort cd = !(cd.k_hooked_sort)
let incr_k_hooked_sort cd = incr cd.k_hooked_sort

let get_real_sort cd = !(cd.real_sort)
let incr_real_sort cd = incr cd.real_sort

let get_k_symbol cd = !(cd.k_symbol)
let incr_k_symbol cd = incr cd.k_symbol

let get_k_hooked_symbol cd = !(cd.k_hooked_symbol)
let incr_k_hooked_symbol cd = incr cd.k_hooked_symbol

let get_real_symbol cd = !(cd.real_symbol)
let incr_real_symbol cd = incr cd.real_symbol

let get_real_induc cd = !(cd.real_induc)
let incr_real_induc cd = incr cd.real_induc

let get_k_alias cd = !(cd.k_alias)
let incr_k_alias cd = incr cd.k_alias

let get_real_alias cd = !(cd.real_alias)
let incr_real_alias cd = incr cd.real_alias

let get_k_rule cd = !(cd.k_rule)
let incr_k_rule cd = incr cd.k_rule

let get_real_rule cd = !(cd.real_rule)
let incr_real_rule cd = incr cd.real_rule

let get_k_axiom cd = !(cd.k_axiom)
let incr_k_axiom cd = incr cd.k_axiom

let get_real_axiom cd = !(cd.real_axiom)
let incr_real_axiom cd = incr cd.real_axiom

(** [reset_count_data i] returns a value of type "count_data" where all internal values are initialised at [i]. *)
let reset_count_data : int -> count_data = fun i ->
  { k_import = ref i      ; real_import = ref i
  ; k_sort   = ref i
  ; k_hooked_sort = ref i ; real_sort   = ref i
  ; k_symbol = ref i
  ; k_hooked_symbol = ref i ; real_symbol = ref i
  ; real_induc = ref i
  ; k_alias = ref i         ; real_alias  = ref i
  ; k_rule  = ref i         ; real_rule   = ref i
  ; k_axiom = ref i         ; real_axiom  = ref i }

let extract_info cd =
  [ (get_real_import cd, Some (get_k_import cd), "import", "imports")

  ; (get_real_sort cd,   Some (get_k_sort cd),   "sort", "sorts")
  ; (0, Some (get_k_hooked_sort cd), "hooked sort", "hooked sorts")

  ; (get_real_symbol cd, Some (get_k_symbol cd), "symbol", "symbols")
  ; (0, Some (get_k_hooked_symbol cd), "hooked symbol", "hooked symbols")
  ; (get_real_induc cd,  Some 0,          "inductive type", "inductive types")

  ; (get_real_alias cd,  Some (get_k_alias cd),  "alias", "alias")
  ; (get_real_rule cd,   Some (get_k_rule cd),   "rule", "rules")
  ; (get_real_axiom cd,  Some (get_k_axiom cd),  "axiom", "axioms") ]

let print_info : int * int option * string * string -> unit = fun (i, j, one, several) ->
  (* Format.fprintf (colorize Format.std_formatter) "Hello!" *)
  let denomi = match j with
     | None -> "?"
     | Some x -> string_of_int x
  in
  if j = Some 0 && i = 0
  then ()
  else
    if i < 2
    then Format.printf "%i / %s %s translated.\n" i denomi one
    else Format.printf "%i / %s %s translated.\n" i denomi several

let print_count_data : count_data -> unit = fun cd -> List.iter print_info (extract_info cd)

let print_header_kamelo : unit -> unit = fun () ->
  Format.printf (gre "-------------------- Welcome to Kamelo ---------------------\n")
let print_header_file filename =
  Format.printf (blu "--- Translation of %s\n") filename

let print_nb_total_commands nb = Format.printf (red "There are %i commands\n") nb

let separator = "------------------------------------------------------------\n"
let print_footer_file : unit -> unit = fun () -> Format.printf "%s" separator

let print_footer_kamelo : unit -> unit = fun () -> Format.printf (gre "%s") separator

let print_module_message : string -> int -> count_data -> unit = fun filename nb cd ->
  print_header_file filename;
  print_nb_total_commands nb;
  print_count_data cd;
  print_footer_file ();
