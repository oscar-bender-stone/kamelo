type name = string
type sort = name
type quant_var = name
type param = S of sort | Q of quant_var

type attribut =
 | Topcellinit  of param list * string
 | Left         of param list * string
 | Right        of param list * string
 | Priorities   of param list * string
 | Subsort      of param list * string
 | Functional   of param list * string
 | Function     of param list * string
 | Constructor  of param list * string
 | Injective    of param list * string
 | Predicate    of param list * string
 | Assoc        of param list * string
 | Comm         of param list * string
 | Idem         of param list * string
 | Unit         of param list * string
 | Element      of param list * string
 | Concat       of param list * string
 | Owise        of param list * string
 | Topcell      of param list * string
 | Cell         of param list * string
 | Maincell     of param list * string
 | Cellname     of param list * string
 | Cellfragment of param list * string
 | Celloptabst  of param list * string
 | Color        of param list * string
 | Latex        of param list * string
 | Nothread     of param list * string
 | Hook         of param list * string
 | Token        of param list * string
 | Klabel       of param list * string
 | Terminals    of param list * string
 | Index        of param list * string
 | SMTlib       of param list * string
 | Format       of param list * string
 | StartLine    of param list * string
 | StartCol     of param list * string
 | Projection   of param list * string
 | Initializer  of param list * string
 | Sortinject   of param list * string
 | Keyword      of param list * string
 | Hasdomainval of param list * string
 | Unique       of param list * string
 | Location     of param list * string
 | Source       of param list * string
 | Production   of param list * string

type symbol = name * quant_var list * param list * param

type predicat =
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
 | Predicat of predicat

type def = A of axiom | D of name * quant_var

type alias = symbol * (name * quant_var list * (name * param) list * def)

type command_aux =
 | Sort     of sort
 | H_sort   of sort
 | Symbol   of symbol
 | H_symbol of symbol
 | Alias    of alias
 | Axiom    of quant_var list * axiom

type command = command_aux * attribut list

type import = name * attribut list

type modu = name * import list * command list * attribut list

type file = attribut list * modu list
