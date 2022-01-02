type name = string
type sort = name
type quant_var = name
type param = S of sort | Q of quant_var

type data_attr = param list * string list

type attribute =
 (* USEFUL attributes *)
 | Assoc        of data_attr
 | Comm         of data_attr
 | Idem         of data_attr
 | Unit         of data_attr

 | Strict       of data_attr
 | Seqstrict    of data_attr

 | Cool         of data_attr
 | CoolLike     of data_attr
 | Heat         of data_attr
 | Structural   of data_attr

 | Simpl        of data_attr

 | Left         of data_attr
 | Right        of data_attr
 | Priorities   of data_attr

 | Constructor  of data_attr
 | Injective    of data_attr
 | Predicate    of data_attr

 | Functional   of data_attr
 | Function     of data_attr

 | Anywhere     of data_attr
 | Owise        of data_attr

 | Subsort      of data_attr
 | Projection   of data_attr
 | Initializer  of data_attr


 (* USELESS attributes
 | Topcellinit  of data_attr
 | Topcell      of data_attr
 | Cell         of data_attr
 | Maincell     of data_attr
 | Cellname     of data_attr
 | Cellfragment of data_attr
 | Celloptabst  of data_attr
 (* ... *)
 | Latex        of data_attr
 | Color        of data_attr
 | Colors       of data_attr
 | Prefer       of data_attr
 | Nothread     of data_attr
 | Hook         of data_attr

 | SMTlib       of data_attr
 | SMThook      of data_attr
 | Format       of data_attr

 | StartLine    of data_attr
 | StartCol     of data_attr

 | Token        of data_attr
 | Klabel       of data_attr
 | Terminals    of data_attr
 | Index        of data_attr

 | Keyword      of data_attr
 | Unique       of data_attr
 | Location     of data_attr
 | Source       of data_attr
 | Production   of data_attr

 | Element      of data_attr
 | Concat       of data_attr

 | Sortinject   of data_attr
 | Hasdomainval of data_attr *)

 | Other        of string * data_attr

type symbol = name * quant_var list * param list * param

type predicate =
 | Sym of name * param list * axiom list
 | Var of name * param
and axiom =
 | Equals of param list * axiom * axiom
 | Exists of param list * (name * param) * axiom
 | And of param list * axiom * axiom
 | Or  of param list * axiom * axiom
 | Not of param list * axiom
 | Implies of param list * axiom * axiom
 | Bottom of param list
 | Top of param list
 | Rewrites of param list * axiom * axiom
 | In of param list * (name * param) * axiom
 | Dom_val of sort * name
 | Ceil of param list * axiom
 | Predicate of predicate

type def = A of axiom | D of name * quant_var

type alias = symbol * (name * quant_var list * (name * param) list * def)

type kommand_aux =
 | Sort     of sort
 | H_sort   of sort
 | Symbol   of symbol
 | H_symbol of symbol
 | Alias    of alias
 | Axiom    of quant_var list * axiom

type kommand = kommand_aux * attribute list

type import = name * attribute list

type kmodule = name * import list * kommand list * attribute list

type file =
   F_sem  of attribute list * kmodule list
 | F_exec of axiom
