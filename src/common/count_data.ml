
type count_data = { k_import : int ref        ; real_import : int ref
                  ; k_sort   : int ref
                  ; k_hooked_sort : int ref   ; real_sort   : int ref
                  ; k_symbol : int ref
                  ; k_hooked_symbol : int ref ; real_symbol : int ref
                  ; real_induc : int ref
                  ; k_alias : int ref         ; real_alias  : int ref
                  ; k_rule  : int ref         ; real_rule   : int ref
                  ; k_axiom : int ref         ; real_axiom  : int ref
                  ; k_ax_subsort : int ref
                  ; k_ax_predicat : int ref
                  ; k_ax_projection : int ref
                  ; k_ax_functional : int ref
                  ; k_ax_constructor : int ref
                  ; k_ax_assoc : int ref
                  ; k_ax_comm  : int ref
                  ; k_ax_idem  : int ref
                  ; k_ax_unit  : int ref
                  ; k_ax_initializer : int ref
                  ; k_ax_owise : int ref
                  ; k_ax_rule  : int ref
                  ; k_ax_without_attr  : int ref
                  ; k_ax_with_one_attr : int ref
                  ; k_ax_several_attr  : int ref }

let get_k_import  cd = !(cd.k_import)
let set_k_import  cd i = cd.k_import := i
let incr_k_import cd = incr cd.k_import

let get_real_import  cd = !(cd.real_import)
let incr_real_import cd = incr cd.real_import

let get_k_sort  cd = !(cd.k_sort)
let incr_k_sort cd = incr cd.k_sort

let get_k_hooked_sort  cd = !(cd.k_hooked_sort)
let incr_k_hooked_sort cd = incr cd.k_hooked_sort

let get_real_sort  cd = !(cd.real_sort)
let incr_real_sort cd = incr cd.real_sort

let get_k_symbol  cd = !(cd.k_symbol)
let incr_k_symbol cd = incr cd.k_symbol

let get_k_hooked_symbol  cd = !(cd.k_hooked_symbol)
let incr_k_hooked_symbol cd = incr cd.k_hooked_symbol

let get_real_symbol  cd = !(cd.real_symbol)
let incr_real_symbol cd = incr cd.real_symbol

let get_real_induc  cd = !(cd.real_induc)
let incr_real_induc cd = incr cd.real_induc

let get_k_alias  cd = !(cd.k_alias)
let incr_k_alias cd = incr cd.k_alias

let get_real_alias  cd = !(cd.real_alias)
let incr_real_alias cd = incr cd.real_alias

let get_k_rule  cd = !(cd.k_rule)
let incr_k_rule cd = incr cd.k_rule

let get_real_rule  cd = !(cd.real_rule)
let incr_real_rule cd = incr cd.real_rule

let get_k_axiom  cd = !(cd.k_axiom)
let incr_k_axiom cd = incr cd.k_axiom

let get_real_axiom  cd = !(cd.real_axiom)
let incr_real_axiom cd = incr cd.real_axiom

let get_k_ax_subsort  cd = !(cd.k_ax_subsort)
let incr_k_ax_subsort cd = incr cd.k_ax_subsort

let get_k_ax_predicat  cd = !(cd.k_ax_predicat)
let incr_k_ax_predicat cd = incr cd.k_ax_predicat

let get_k_ax_projection  cd = !(cd.k_ax_projection)
let incr_k_ax_projection cd = incr cd.k_ax_projection

let get_k_ax_functional  cd = !(cd.k_ax_functional)
let incr_k_ax_functional cd = incr cd.k_ax_functional

let get_k_ax_constructor  cd = !(cd.k_ax_constructor)
let incr_k_ax_constructor cd = incr cd.k_ax_constructor

let get_k_ax_assoc  cd = !(cd.k_ax_assoc)
let incr_k_ax_assoc cd = incr cd.k_ax_assoc

let get_k_ax_comm  cd = !(cd.k_ax_comm)
let incr_k_ax_comm cd = incr cd.k_ax_comm

let get_k_ax_idem  cd = !(cd.k_ax_idem)
let incr_k_ax_idem cd = incr cd.k_ax_idem

let get_k_ax_unit  cd = !(cd.k_ax_unit)
let incr_k_ax_unit cd = incr cd.k_ax_unit

let get_k_ax_initializer  cd = !(cd.k_ax_initializer)
let incr_k_ax_initializer cd = incr cd.k_ax_initializer

let get_k_ax_owise  cd = !(cd.k_ax_owise)
let incr_k_ax_owise cd = incr cd.k_ax_owise

let get_k_ax_rule  cd = !(cd.k_ax_rule)
let incr_k_ax_rule cd = incr cd.k_ax_rule

let get_k_ax_without_attr  cd = !(cd.k_ax_without_attr)
let incr_k_ax_without_attr cd = incr cd.k_ax_without_attr

let get_k_ax_with_one_attr  cd = !(cd.k_ax_with_one_attr)
let incr_k_ax_with_one_attr cd = incr cd.k_ax_with_one_attr

let get_k_ax_several_attr  cd = !(cd.k_ax_several_attr)
let incr_k_ax_several_attr cd = incr cd.k_ax_several_attr

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
  ; k_axiom = ref i         ; real_axiom  = ref i
  ; k_ax_subsort = ref i
  ; k_ax_predicat = ref i
  ; k_ax_projection = ref i
  ; k_ax_functional = ref i
  ; k_ax_constructor = ref i
  ; k_ax_assoc = ref i
  ; k_ax_comm  = ref i
  ; k_ax_idem  = ref i
  ; k_ax_unit  = ref i
  ; k_ax_initializer = ref i
  ; k_ax_owise = ref i
  ; k_ax_rule  = ref i
  ; k_ax_without_attr  = ref i
  ; k_ax_with_one_attr = ref i
  ; k_ax_several_attr  = ref i }

let extract_info cd =
  [ (Some (get_real_import cd), Some (get_k_import cd),          "import", "imports")

  ; (Some (get_real_sort cd),   Some (get_k_sort cd),            "sort", "sorts")
  ; (Some 0,                    Some (get_k_hooked_sort cd),     "hooked sort", "hooked sorts")

  ; (Some (get_real_symbol cd), Some (get_k_symbol cd),          "symbol", "symbols")
  ; (Some 0,                    Some (get_k_hooked_symbol cd),   "hooked symbol", "hooked symbols")
  ; (Some (get_real_induc cd),  Some 0,                          "inductive type", "inductive types")

  ; (Some (get_real_alias cd),  Some (get_k_alias cd),           "alias", "alias")
  ; (Some (get_real_rule cd),   Some (get_k_rule cd),            "rule", "rules")
  ; (Some (get_real_axiom cd),  Some (get_k_axiom cd),           "axiom", "axioms")
  ; (None,                      Some (get_k_ax_subsort cd),      "subsort one", "subsort one")
  ; (None,                      Some (get_k_ax_predicat cd),     "predicat one", "predicat one")
  ; (None,                      Some (get_k_ax_projection cd),   "projection one", "projection one")
  ; (None,                      Some (get_k_ax_functional cd),   "functional one", "functional one")
  ; (None,                      Some (get_k_ax_constructor cd),  "constructor one", "constructor one")
  ; (None,                      Some (get_k_ax_assoc cd),        "associative one", "associative one")
  ; (None,                      Some (get_k_ax_comm cd),         "commutative one", "commutative one")
  ; (None,                      Some (get_k_ax_idem cd),         "idempotence one", "idempotence one")
  ; (None,                      Some (get_k_ax_unit cd),         "identity one", "identity one")
  ; (None,                      Some (get_k_ax_initializer cd),  "initializer one", "initializer one")
  ; (None,                      Some (get_k_ax_owise cd),        "otherwise one", "otherwise one")
  ; (None,                      Some (get_k_ax_rule cd),         "rewriting rule", "rewriting rules")
  ; (None,                      Some (get_k_ax_without_attr  cd), "without attribute", "without attribute")
  ; (None,                      Some (get_k_ax_with_one_attr cd), "with one attribute", "with one attribute")
  ; (None,                      Some (get_k_ax_several_attr  cd), "with several attributes", "with several attributes") ]
