
type cd_exists_ax =
  { total : int ref ; subsort : int ref ; total_attribute : int ref }

let reset_cd_exists_ax : int -> cd_exists_ax = fun i ->
  { total = ref i ; subsort = ref i ; total_attribute = ref i }

type cd_equals_ax =
  { total : int ref ;
    assoc : int ref ; comm : int ref ;
    idem  : int ref ; unit : int ref }

let reset_cd_equals_ax : int -> cd_equals_ax = fun i ->
  { total = ref i ;
    assoc = ref i ; comm = ref i ; idem = ref i ; unit = ref i }

type cd_or_ax = { total : int ref ; constructor : int ref }

let reset_cd_or_ax : int -> cd_or_ax = fun i ->
  { total = ref i ; constructor = ref i }

type cd_bottom_ax = { total : int ref ; constructor : int ref }

let reset_cd_bottom_ax : int -> cd_bottom_ax = fun i ->
  { total = ref i ; constructor = ref i }

type cd_not_ax = { total : int ref ; constructor : int ref }

let reset_cd_not_ax : int -> cd_not_ax = fun i ->
  { total = ref i ; constructor = ref i }

type cd_implies_ax =
  { total : int ref ;
    constructor : int ref ; init : int ref ; projection : int ref ;
    predicate_false : int ref ; predicate_true : int ref ;
    owise : int ref ; with_one_attr : int ref }

let reset_cd_implies_ax : int -> cd_implies_ax = fun i ->
  { total = ref i ;
    constructor = ref i ; init = ref i ; projection = ref i ;
    predicate_false = ref i ; predicate_true = ref i ;
    owise = ref i ; with_one_attr = ref i }

type cd_rewriting_ax =
  { total : int ref ;
    heating : int ref ; cooling : int ref ; semantic : int ref }

let reset_cd_rewriting_ax : int -> cd_rewriting_ax = fun i ->
  { total = ref i ;
    heating = ref i ; cooling = ref i ; semantic = ref i }

type count_data_k =
  { import : int ref ;
    sort   : int ref ; hooked_sort : int ref ;
    symbol : int ref ; hooked_symbol : int ref ;
    alias  : int ref ;
    axiom  : int ref ;
      exists_ax    : cd_exists_ax    ;
      equals_ax    : cd_equals_ax    ;
      or_ax        : cd_or_ax        ;
      bottom_ax    : cd_bottom_ax    ;
      not_ax       : cd_not_ax       ;
      implies_ax   : cd_implies_ax   ;
      rewriting_ax : cd_rewriting_ax ;
      ax_without_attr : int ref ;
      ax_several_attr : int ref ;
    claim  : int ref }

(** [reset_count_data_k i] returns a value of type "count_data_k"
    where all internal values are initialised at [i]. *)
let reset_count_data_k : int -> count_data_k = fun i ->
  { import = ref i ; sort = ref i ; hooked_sort = ref i ;
    symbol = ref i ; hooked_symbol = ref i ; alias = ref i ;
    axiom  = ref i ;
    exists_ax = reset_cd_exists_ax i ; equals_ax = reset_cd_equals_ax i ;
    or_ax = reset_cd_or_ax i ; bottom_ax = reset_cd_bottom_ax i ;
    not_ax = reset_cd_not_ax i ; implies_ax = reset_cd_implies_ax i ;
    rewriting_ax = reset_cd_rewriting_ax i ; ax_without_attr = ref i ;
    ax_several_attr = ref i ;
    claim = ref i }

type count_data_dk =
  { import : int ref ; symbol : int ref ; hooked_symbol : int ref ;
    additional_sym : int ref ; inductive : int ref ; rule  : int ref }

(** [reset_count_data_dk i] returns a value of type "count_data_dk"
    where all internal values are initialised at [i]. *)
let reset_count_data_dk : int -> count_data_dk = fun i ->
  { import = ref i ; symbol = ref i ; hooked_symbol = ref i ;
    additional_sym = ref i ; inductive = ref i ; rule = ref i }

type count_data = { k : count_data_k ; dk : count_data_dk }

(** [reset_count_data i] returns a value of type "count_data"
    where all internal values are initialised at [i]. *)
let reset_count_data : int -> count_data = fun i ->
  { k = reset_count_data_k i ; dk = reset_count_data_dk i }

(** Data about the K commands *)

let get_k_import  cd = !(cd.k.import)
let set_k_import  cd i = cd.k.import := i
let incr_k_import cd = incr cd.k.import

let get_k_sort  cd = !(cd.k.sort)
let incr_k_sort cd = incr cd.k.sort

let get_k_hooked_sort  cd = !(cd.k.hooked_sort)
let incr_k_hooked_sort cd = incr cd.k.hooked_sort

let get_k_symbol  cd = !(cd.k.symbol)
let incr_k_symbol cd = incr cd.k.symbol

let get_k_hooked_symbol  cd = !(cd.k.hooked_symbol)
let incr_k_hooked_symbol cd = incr cd.k.hooked_symbol

let get_k_alias  cd = !(cd.k.alias)
let incr_k_alias cd = incr cd.k.alias

let get_k_axiom  cd = !(cd.k.axiom)
let incr_k_axiom cd = incr cd.k.axiom

let get_k_ax_without_attr  cd = !(cd.k.ax_without_attr)
let incr_k_ax_without_attr cd = incr cd.k.ax_without_attr

let get_k_ax_several_attr  cd = !(cd.k.ax_several_attr)
let incr_k_ax_several_attr cd = incr cd.k.ax_several_attr

let get_k_claim  cd = !(cd.k.claim)
let incr_k_claim cd = incr cd.k.claim

(** Data about the K axioms *)

(* Exists one *)

let get_k_exists_ax cd  = !(cd.k.exists_ax.total)
let incr_k_exists_ax cd = incr cd.k.exists_ax.total

let get_k_ax_subsort  cd = !(cd.k.exists_ax.subsort)
let incr_k_ax_subsort cd = incr cd.k.exists_ax.subsort

let get_k_ax_total  cd = !(cd.k.exists_ax.total_attribute)
let incr_k_ax_total cd = incr cd.k.exists_ax.total_attribute

(* Equals one *)

let get_k_equals_ax cd  = !(cd.k.equals_ax.total)
let incr_k_equals_ax cd = incr cd.k.equals_ax.total

let get_k_ax_assoc  cd = !(cd.k.equals_ax.assoc)
let incr_k_ax_assoc cd = incr cd.k.equals_ax.assoc

let get_k_ax_comm  cd = !(cd.k.equals_ax.comm)
let incr_k_ax_comm cd = incr cd.k.equals_ax.comm

let get_k_ax_idem  cd = !(cd.k.equals_ax.idem)
let incr_k_ax_idem cd = incr cd.k.equals_ax.idem

let get_k_ax_unit  cd = !(cd.k.equals_ax.unit)
let incr_k_ax_unit cd = incr cd.k.equals_ax.unit

(* Or one *)

let get_k_or_ax  cd = !(cd.k.or_ax.total)
let incr_k_or_ax cd = incr cd.k.or_ax.total

let get_k_or_ax_junk_constructor  cd = !(cd.k.or_ax.constructor)
let incr_k_or_ax_junk_constructor cd = incr cd.k.or_ax.constructor

(* Bottom one *)

let get_k_bottom_ax  cd = !(cd.k.bottom_ax.total)
let incr_k_bottom_ax cd = incr cd.k.bottom_ax.total

let get_k_bottom_ax_junk_constructor  cd = !(cd.k.bottom_ax.constructor)
let incr_k_bottom_ax_junk_constructor cd = incr cd.k.bottom_ax.constructor

(* Not one *)

let get_k_not_ax  cd = !(cd.k.not_ax.total)
let incr_k_not_ax cd = incr cd.k.not_ax.total

let get_k_not_ax_diff_constructor  cd = !(cd.k.not_ax.constructor)
let incr_k_not_ax_diff_constructor cd = incr cd.k.not_ax.constructor

(* Implies one *)

let get_k_implies_ax  cd = !(cd.k.implies_ax.total)
let incr_k_implies_ax cd = incr cd.k.implies_ax.total

let get_k_ax_same_constructor  cd = !(cd.k.implies_ax.constructor)
let incr_k_ax_same_constructor cd = incr cd.k.implies_ax.constructor

let get_k_ax_initializer  cd = !(cd.k.implies_ax.init)
let incr_k_ax_initializer cd = incr cd.k.implies_ax.init

let get_k_ax_projection  cd = !(cd.k.implies_ax.projection)
let incr_k_ax_projection cd = incr cd.k.implies_ax.projection

let get_k_ax_predicate_false  cd = !(cd.k.implies_ax.predicate_false)
let incr_k_ax_predicate_false cd = incr cd.k.implies_ax.predicate_false

let get_k_ax_predicate_true  cd = !(cd.k.implies_ax.predicate_true)
let incr_k_ax_predicate_true cd = incr cd.k.implies_ax.predicate_true
(* Nous ne savons pas si ces axiomes commence forcément par \implies *)

let get_k_ax_owise  cd = !(cd.k.implies_ax.owise)
let incr_k_ax_owise cd = incr cd.k.implies_ax.owise

let get_k_ax_with_one_attr  cd = !(cd.k.implies_ax.with_one_attr)
let incr_k_ax_with_one_attr cd = incr cd.k.implies_ax.with_one_attr

(* Rewriting one *)

let get_k_rewriting_ax  cd = !(cd.k.rewriting_ax.total)
let incr_k_rewriting_ax cd = incr cd.k.rewriting_ax.total

let get_k_ax_heating  cd = !(cd.k.rewriting_ax.heating)
let incr_k_ax_heating cd = incr cd.k.rewriting_ax.heating

let get_k_ax_cooling  cd = !(cd.k.rewriting_ax.cooling)
let incr_k_ax_cooling cd = incr cd.k.rewriting_ax.cooling

let get_k_ax_semantic  cd = !(cd.k.rewriting_ax.semantic)
let incr_k_ax_semantic cd = incr cd.k.rewriting_ax.semantic

(** Data about the Dedukti commands *)

let get_real_import  cd = !(cd.dk.import)
let incr_real_import cd = incr cd.dk.import

let get_real_symbol  cd = !(cd.dk.symbol)
let incr_real_symbol cd = incr cd.dk.symbol

let get_additional_symbol  cd = !(cd.dk.additional_sym)
let incr_additional_symbol cd = incr cd.dk.additional_sym

let get_real_induc  cd = !(cd.dk.inductive)
let incr_real_induc cd = incr cd.dk.inductive

let get_real_rule  cd = !(cd.dk.rule)
let incr_real_rule cd = incr cd.dk.rule

let extract_info_before cd =
  [ (0, get_k_import cd,          "import",          "imports")

  ; (0, get_k_sort cd,            "sort",            "sorts")
  ; (0, get_k_hooked_sort cd,     "hooked sort",     "hooked sorts")

  ; (0, get_k_symbol cd,          "symbol",          "symbols")
  ; (0, get_k_hooked_symbol cd,   "hooked symbol",   "hooked symbols")

  ; (0, get_k_alias cd,           "alias",           "alias")
  ; (0, get_k_axiom cd,           "axiom",           "axioms")

  ; (1, get_k_exists_ax cd,           "exists-axiom", "exists-axioms")
  ; (2, get_k_ax_subsort cd,          "subsort one",  "subsort one")
  ; (2, get_k_ax_total cd,            "total one",    "total one")

  ; (1, get_k_equals_ax cd,           "equals-axiom",      "equals-axioms")
  ; (2, get_k_ax_assoc cd,            "associative one",   "associative one")
  ; (2, get_k_ax_comm cd,             "commutative one",   "commutative one")
  ; (2, get_k_ax_unit cd,             "unit/identity one", "unit/identity one")
  ; (2, get_k_ax_idem cd,             "idempotence one",   "idempotence one")

  ; (1, get_k_or_ax cd,               "or-axiom",        "or-axioms")
  ; (2, get_k_or_ax_junk_constructor cd,
                                      "junk constructor", "junk constructor")

  ; (1, get_k_bottom_ax cd,           "bottom-axiom",     "bottom-axioms")
  ; (2, get_k_bottom_ax_junk_constructor cd,
                                      "junk constructor", "junk constructor")

  ; (1, get_k_not_ax cd,              "not-axiom",        "not-axioms")
  ; (2, get_k_not_ax_diff_constructor cd,
                                      "diff constructor", "diff constructor")

  ; (1, get_k_implies_ax cd,          "implies-axiom",   "implies-axioms")
  ; (2, get_k_ax_same_constructor cd, "same constructor",           "same constructor")
  ; (2, get_k_ax_initializer cd,      "initializer one",            "initializer one")
  ; (2, get_k_ax_projection cd,       "projection one",             "projection one")
  ; (2, get_k_ax_predicate_false cd,  "predicate one (case false)", "predicate one (case false)")
  ; (2, get_k_ax_predicate_true cd,   "predicate one (case true)",  "predicate one (case true)")
  ; (2, get_k_ax_owise cd,            "otherwise one",              "otherwise one")
  ; (2, get_k_ax_without_attr  cd,    "without attribute",          "without attribute")
  ; (2, get_k_ax_with_one_attr cd,    "with one attribute",         "with one attribute")

  ; (1, get_k_rewriting_ax cd,        "rewriting one", "rewriting one")
  ; (2, get_k_ax_heating   cd,        "heating rule",  "heating rules")
  ; (2, get_k_ax_cooling   cd,        "cooling rule",  "cooling rules")
  ; (2, get_k_ax_semantic  cd,        "semantic rule", "semantic rules")


  ; (1, get_k_ax_several_attr  cd,    "with several attributes", "with several attributes")

  ; (0, get_k_claim cd,           "claim",           "claims") ]

let extract_info_after cd =
  [ (0, get_real_import cd,          "import",            "imports")
  ; (0, get_real_symbol cd,          "symbol",            "symbols")
  ; (0, get_additional_symbol cd,    "additional symbol", "additional symbols")
  ; (0, get_real_induc cd,           "inductive type",    "inductive types")
  ; (0, get_real_rule cd,            "rewriting rule",    "rewriting rules") ]
