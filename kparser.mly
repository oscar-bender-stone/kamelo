%{
    open Type
%}

%token MODULE
%token ENDMODULE
%token IMPORT

%token SORT
%token H_SORT
%token SYMBOL
%token H_SYMBOL
%token ALIAS
%token WHERE
%token AXIOM

%token L_CURLY_BRA          //      {
%token R_CURLY_BRA          //      }
%token L_SQUARE_BRA         //      [
%token R_SQUARE_BRA         //      ]
%token L_PAREN              //      (
%token R_PAREN              //      )
%token COLON                //      :
%token COMMA                //      ,
%token DEF                  //      :=

%token EQUALS
%token EXISTS
%token AND
%token OR
%token NOT
%token IMPLIES
%token BOTTOM
%token TOP
%token REWRITES
%token IN
%token DOM_VAL

%token TOPCELLINIT
%token LEFT
%token RIGHT
%token PRIORITIES
%token SUBSORT
%token FUNCTIONAL
%token FUNCTION
%token CONSTRUCTOR
%token INJECTIVE
%token PREDICATE
%token ASSOC
%token COMM
%token IDEM
%token UNIT
%token ELEMENT
%token CONCAT
%token OWISE
%token TOPCELL
%token CELL
%token MAINCELL
%token CELLNAME
%token CELLFRAGMENT
%token CELLOPTABST
%token COLOR
%token LATEX
%token NOTHREAD
%token HOOK
%token TOKEN
%token KLABEL
%token TERMINALS
%token INDEX
%token SMTLIB
%token FORMAT
%token STARTLINE
%token STARTCOLUMN
%token PROJECTION
%token INITIALIZER
%token SORTINJECT
%token HASDOMAINVAL
%token KEYWORD
%token UNIQUE
%token LOCATION
%token SOURCE
%token PRODUCTION

%token <string> IDENT       // Identifiant
%token <string> STRING
%token EOF                  // End of file

%start file

%type <Type.name> name
%type <Type.quant_var> quant_var
%type <Type.param> param
%type <Type.sort> sort

%type <Type.symbol> symbol
%type <Type.attribut> attribut
%type <Type.command> command
%type <Type.import> import
%type <Type.modu list> modules
//%type <Type.axiom> axiom

%type <Type.file> file
%%

name:
  | IDENT { $1 }

sort:
  | name L_CURLY_BRA R_CURLY_BRA { $1 }

quant_var:
  | name { $1 }

quant_var_list:
  | separated_list(COMMA, quant_var) { $1 }

param:
  | sort      { S $1 }
  | quant_var { Q $1 }

param_list:
  | separated_list(COMMA, param) { $1 }

symbol: //  See the symbol inj
  | name L_CURLY_BRA quant_var_list R_CURLY_BRA L_PAREN param_list R_PAREN COLON param
     { ($1, $3, $6, $9) }

body_where:
  | axiom                { A $1       }
 // | name COLON quant_var { D ($1, $3) }

d_tmp:
  | name COLON param { ($1, $3) }

def:
  | name L_CURLY_BRA quant_var_list R_CURLY_BRA
         L_PAREN separated_list(COMMA, d_tmp) R_PAREN DEF body_where
     { ($1, $3, $6, $9) }

op_const:
  | L_CURLY_BRA param_list R_CURLY_BRA L_PAREN R_PAREN { $2 }

op_una:
  | L_CURLY_BRA param_list R_CURLY_BRA L_PAREN axiom R_PAREN { $2, $5 }

op_bin:
  | L_CURLY_BRA param_list R_CURLY_BRA L_PAREN axiom COMMA axiom R_PAREN { ($2, $5, $7) }

op_quant:
  | L_CURLY_BRA param_list R_CURLY_BRA L_PAREN name COLON param COMMA axiom R_PAREN
     { ($2, ($5, $7) , $9) }

predicat:
  | name L_CURLY_BRA param_list R_CURLY_BRA
         L_PAREN separated_list(COMMA, axiom) R_PAREN { Sym ($1, $3, $6) }
  | name COLON param                                  { Var ($1, $3)     }

axiom:
  | EQUALS   op_bin   { let a1,a2,a3 = $2 in Equals(a1, a2, a3) }
  | EXISTS   op_quant { let pl, q, a = $2 in Exists (pl, q, a) }
  | AND      op_bin   { let a1,a2,a3 = $2 in And(a1, a2, a3) }
  | OR       op_bin   { let a1,a2,a3 = $2 in Or(a1, a2, a3) }
  | NOT      op_una   { let a1, a2   = $2 in Not(a1, a2) }
  | IMPLIES  op_bin   { let a1,a2,a3 = $2 in Implies(a1, a2, a3) }
  | BOTTOM   op_const { Bottom $2 }
  | TOP      op_const { Top $2 }
  | REWRITES op_bin   { let a1,a2,a3 = $2 in Rewrites(a1, a2, a3) }
  | IN       op_quant { let pl, q, a = $2 in In (pl, q, a) }
  | DOM_VAL  L_CURLY_BRA sort R_CURLY_BRA L_PAREN STRING R_PAREN { Dom_val ($3, $6) }
  //| name L_CURLY_BRA param_list R_CURLY_BRA L_PAREN separated_list(COMMA, axiom) R_PAREN
  //   { Sym ($1, $3, $6) }
  //| name COLON param { Var ($1, $3) }
  | predicat           { Predicat $1 }

command:
  | SORT     sort        attributs { Sort $2, $3 }
  | H_SORT   sort        attributs { H_sort $2, $3 }
  | SYMBOL   symbol      attributs { Symbol $2, $3 }
  | H_SYMBOL symbol      attributs { H_symbol $2, $3 }
  | ALIAS symbol WHERE def attributs { Alias ($2, $4), $5 }
  | AXIOM L_CURLY_BRA quant_var_list R_CURLY_BRA axiom attributs { Axiom ($3, $5), $6 }

import:
  | IMPORT name attributs { ($2, $3) }

modules:
  | MODULE name import* command* ENDMODULE attributs modules { ($2, $3, $4, $6) :: $7 }
  | EOF                                                      {          []            }

instance_symbol:
  | name L_CURLY_BRA R_CURLY_BRA L_PAREN R_PAREN { $1 }
  | STRING { $1 }
  |        { "" }

attri_params:
  | L_CURLY_BRA param_list R_CURLY_BRA L_PAREN instance_symbol R_PAREN
      { ($2, $5) }

attribut:
  | TOPCELLINIT    attri_params { let a1,a2 = $2 in Topcellinit (a1, a2) }
  | LEFT           attri_params { let a1,a2 = $2 in Left(a1, a2)         }
  | RIGHT          attri_params { let a1,a2 = $2 in Right(a1, a2)        }
  | PRIORITIES     attri_params { let a1,a2 = $2 in Priorities(a1, a2)   }
  | SUBSORT        attri_params { let a1,a2 = $2 in Subsort(a1, a2)      }
  | FUNCTIONAL     attri_params { let a1,a2 = $2 in Functional(a1, a2)   }
  | FUNCTION       attri_params { let a1,a2 = $2 in Function(a1, a2)     }
  | CONSTRUCTOR    attri_params { let a1,a2 = $2 in Constructor(a1, a2)  }
  | INJECTIVE      attri_params { let a1,a2 = $2 in Injective(a1, a2)    }
  | PREDICATE      attri_params { let a1,a2 = $2 in Predicate(a1, a2)    }
  | ASSOC          attri_params { let a1,a2 = $2 in Assoc(a1, a2)        }
  | COMM           attri_params { let a1,a2 = $2 in Comm(a1, a2)         }
  | IDEM           attri_params { let a1,a2 = $2 in Idem(a1, a2)         }
  | UNIT           attri_params { let a1,a2 = $2 in Unit(a1, a2)         }
  | ELEMENT        attri_params { let a1,a2 = $2 in Element(a1, a2)      }
  | CONCAT         attri_params { let a1,a2 = $2 in Concat(a1, a2)       }
  | OWISE          attri_params { let a1,a2 = $2 in Owise(a1, a2)        }
  | TOPCELL        attri_params { let a1,a2 = $2 in Topcell(a1, a2)      }
  | CELL           attri_params { let a1,a2 = $2 in Cell(a1, a2)         }
  | MAINCELL       attri_params { let a1,a2 = $2 in Maincell(a1, a2)     }
  | CELLNAME       attri_params { let a1,a2 = $2 in Cellname(a1, a2)     }
  | CELLFRAGMENT   attri_params { let a1,a2 = $2 in Cellfragment(a1, a2) }
  | CELLOPTABST    attri_params { let a1,a2 = $2 in Celloptabst(a1, a2)  }
  | COLOR          attri_params { let a1,a2 = $2 in Color(a1, a2)        }
  | LATEX          attri_params { let a1,a2 = $2 in Latex(a1, a2)        }
  | NOTHREAD       attri_params { let a1,a2 = $2 in Nothread(a1, a2)     }
  | HOOK           attri_params { let a1,a2 = $2 in Hook(a1, a2)         }
  | TOKEN          attri_params { let a1,a2 = $2 in Token(a1, a2)        }
  | KLABEL         attri_params { let a1,a2 = $2 in Klabel(a1, a2)       }
  | TERMINALS      attri_params { let a1,a2 = $2 in Terminals(a1, a2)    }
  | INDEX          attri_params { let a1,a2 = $2 in Index(a1, a2)        }
  | SMTLIB         attri_params { let a1,a2 = $2 in SMTlib(a1, a2)       }
  | FORMAT         attri_params { let a1,a2 = $2 in Format(a1, a2)       }
  | STARTLINE      attri_params { let a1,a2 = $2 in StartLine(a1, a2)    }
  | STARTCOLUMN    attri_params { let a1,a2 = $2 in StartCol(a1, a2)     }
  | PROJECTION     attri_params { let a1,a2 = $2 in Projection(a1, a2)   }
  | INITIALIZER    attri_params { let a1,a2 = $2 in Initializer(a1, a2)  }
  | SORTINJECT     attri_params { let a1,a2 = $2 in Sortinject(a1, a2)   }
  | HASDOMAINVAL   attri_params { let a1,a2 = $2 in Hasdomainval(a1, a2) }
  | KEYWORD        attri_params { let a1,a2 = $2 in Keyword(a1, a2)      }
  | UNIQUE         attri_params { let a1,a2 = $2 in Unique(a1, a2)       }
  | LOCATION       attri_params { let a1,a2 = $2 in Location(a1, a2)     }
  | SOURCE         attri_params { let a1,a2 = $2 in Source(a1, a2)       }
  | PRODUCTION     attri_params { let a1,a2 = $2 in Production(a1, a2)   }

attributs:
  | L_SQUARE_BRA separated_list(COMMA, attribut) R_SQUARE_BRA { $2 }

file:
  | attributs modules { ($1, $2) }
  | EOF               { ([], []) }
