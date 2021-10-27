%{
    open Common.Type
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

// USEFUL attributes
%token ASSOC
%token COMM
%token IDEM
%token UNIT

%token STRICT
%token SEQSTRICT

%token COOL
%token COOLLIKE
%token HEAT
%token STRUCTURAL

%token SIMPLIFICATION

%token LEFT
%token RIGHT
%token PRIORITIES

%token CONSTRUCTOR
%token INJECTIVE
%token PREDICATE

%token FUNCTIONAL
%token FUNCTION

%token ANYWHERE
%token OWISE

%token SUBSORT
%token PROJECTION
%token INITIALIZER

// USELESS attributes
%token TOPCELLINIT
%token TOPCELL
%token CELL
%token MAINCELL
%token CELLNAME
%token CELLFRAGMENT
%token CELLOPTABST

%token EQEQK
%token NOTEQEQK
%token BOOLOP
%token USERLIST

%token IMPURE
%token CONCRETE

%token LATEX
%token COLOR
%token COLORS
%token PREFER
%token NOTHREAD
%token HOOK

%token SMTLIB
%token SMTHOOK
%token FORMAT
%token MACRO
%token RESULT

%token EXIT
%token AVOID
%token RETURNSUNIT

%token STARTLINE
%token STARTCOLUMN

%token TOKEN
%token KLABEL
%token TERMINALS
%token INDEX
%token FRESHGENERATOR

%token KEYWORD
%token UNIQUE
%token LOCATION
%token SOURCE
%token PRODUCTION

%token ELEMENT
%token CONCAT

%token SORTINJECT
%token HASDOMAINVAL

%token <string> IDENT       // Identifiant
%token <string> STRING
%token EOF                  // End of file

%start file

%type <Common.Type.name> name
%type <Common.Type.quant_var> quant_var
%type <Common.Type.param> param
%type <Common.Type.sort> sort

%type <Common.Type.symbol> symbol
%type <Common.Type.attribute> attribute
%type <Common.Type.kommand> kommand
%type <Common.Type.import> import
%type <Common.Type.kmodule list> kmodules
%type <Common.Type.axiom> axiom

%type <Common.Type.file> file
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

predicate:
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
  | predicate           { Predicat $1 }

kommand:
  | SORT     sort        attributes { Sort $2, $3 }
  | H_SORT   sort        attributes { H_sort $2, $3 }
  | SYMBOL   symbol      attributes { Symbol $2, $3 }
  | H_SYMBOL symbol      attributes { H_symbol $2, $3 }
  | ALIAS symbol WHERE def attributes { Alias ($2, $4), $5 }
  | AXIOM L_CURLY_BRA quant_var_list R_CURLY_BRA axiom attributes { Axiom ($3, $5), $6 }

import:
  | IMPORT name attributes { ($2, $3) }

kmodules:
  | MODULE name import* kommand* ENDMODULE attributes kmodules { ($2, $3, $4, $6) :: $7 }
  | EOF                                                        {          []            }

instance_symbol:
  | name L_CURLY_BRA R_CURLY_BRA L_PAREN R_PAREN { $1 }
  | STRING { $1 }
  //|        { "" }

attri_params:
  | L_CURLY_BRA param_list R_CURLY_BRA L_PAREN separated_list(COMMA, instance_symbol) R_PAREN
      { ($2, $5) }

useless_attribute:
  | STRICT         attri_params { } // let a1,a2 = $2 in Strict(a1, a2)       }
  | SEQSTRICT      attri_params { } // let a1,a2 = $2 in Seqstrict(a1, a2)    }

  | COOL           attri_params { } // let a1,a2 = $2 in Cool(a1, a2)         }
  | COOLLIKE       attri_params { } // let a1,a2 = $2 in CoolLike(a1, a2)     }
  | HEAT           attri_params { } // let a1,a2 = $2 in Heat(a1, a2)         }
  | STRUCTURAL     attri_params { } // let a1,a2 = $2 in Structural(a1, a2)   }

  | SIMPLIFICATION attri_params { } // let a1,a2 = $2 in Simpl(a1, a2)        }


  | LEFT           attri_params { } // let a1,a2 = $2 in Left(a1, a2)         }
  | RIGHT          attri_params { } // let a1,a2 = $2 in Right(a1, a2)        }
  | PRIORITIES     attri_params { } // let a1,a2 = $2 in Priorities(a1, a2)   }

  | TOPCELLINIT    attri_params {  } // { let a1,a2 = $2 in Topcellinit (a1, a2) }
  | TOPCELL        attri_params {  } // { let a1,a2 = $2 in Topcell(a1, a2)      }
  | CELL           attri_params {  } // { let a1,a2 = $2 in Cell(a1, a2)         }
  | MAINCELL       attri_params {  } // { let a1,a2 = $2 in Maincell(a1, a2)     }
  | CELLNAME       attri_params {  } // { let a1,a2 = $2 in Cellname(a1, a2)     }
  | CELLFRAGMENT   attri_params {  } // { let a1,a2 = $2 in Cellfragment(a1, a2) }
  | CELLOPTABST    attri_params {  } // { let a1,a2 = $2 in Celloptabst(a1, a2)  }

  | NOTEQEQK       attri_params {  }
  | EQEQK          attri_params {  }
  | BOOLOP         attri_params {  }
  | USERLIST       attri_params {  }

  | IMPURE         attri_params {  }
  | CONCRETE       attri_params {  }

  | LATEX          attri_params {  } // { let a1,a2 = $2 in Latex(a1, a2)        }
  | COLOR          attri_params {  } // { let a1,a2 = $2 in Color(a1, a2)        }
  | COLORS         attri_params {  } // { let a1,a2 = $2 in Colors(a1, a2)       }
  | PREFER         attri_params {  } // { let a1,a2 = $2 in Prefer(a1, a2)       }
  | NOTHREAD       attri_params {  } // { let a1,a2 = $2 in Nothread(a1, a2)     }
  | HOOK           attri_params {  } // { let a1,a2 = $2 in Hook(a1, a2)         }

  | SMTLIB         attri_params {  } // { let a1,a2 = $2 in SMTlib(a1, a2)       }
  | SMTHOOK        attri_params {  } // { let a1,a2 = $2 in SMThook(a1, a2)      }
  | FORMAT         attri_params {  } // { let a1,a2 = $2 in Format(a1, a2)       }
  | MACRO          attri_params {  }
  | RESULT         attri_params {  }

  | EXIT           attri_params {  }
  | AVOID          attri_params {  }
  | RETURNSUNIT    attri_params {  }

  | STARTLINE      attri_params {  } // { let a1,a2 = $2 in StartLine(a1, a2)    }
  | STARTCOLUMN    attri_params {  } // { let a1,a2 = $2 in StartCol(a1, a2)     }

  | TOKEN          attri_params {  } // { let a1,a2 = $2 in Token(a1, a2)        }
  | KLABEL         attri_params {  } // { let a1,a2 = $2 in Klabel(a1, a2)       }
  | TERMINALS      attri_params {  } // { let a1,a2 = $2 in Terminals(a1, a2)    }
  | INDEX          attri_params {  } // { let a1,a2 = $2 in Index(a1, a2)        }
  | FRESHGENERATOR attri_params {  }

  | KEYWORD        attri_params {  } // { let a1,a2 = $2 in Keyword(a1, a2)      }
  | UNIQUE         attri_params {  } // { let a1,a2 = $2 in Unique(a1, a2)       }
  | LOCATION       attri_params {  } // { let a1,a2 = $2 in Location(a1, a2)     }
  | SOURCE         attri_params {  } // { let a1,a2 = $2 in Source(a1, a2)       }
  | PRODUCTION     attri_params {  } // { let a1,a2 = $2 in Production(a1, a2)   }

  | ELEMENT        attri_params {  } // { let a1,a2 = $2 in Element(a1, a2)      }
  | CONCAT         attri_params {  } // { let a1,a2 = $2 in Concat(a1, a2)       }

  | SORTINJECT     attri_params {  } // { let a1,a2 = $2 in Sortinject(a1, a2)   }
  | HASDOMAINVAL   attri_params {  } // { let a1,a2 = $2 in Hasdomainval(a1, a2) }

attribute:
  | ASSOC          attri_params { let a1,a2 = $2 in Assoc(a1, a2)        }
  | COMM           attri_params { let a1,a2 = $2 in Comm(a1, a2)         }
  | IDEM           attri_params { let a1,a2 = $2 in Idem(a1, a2)         }
  | UNIT           attri_params { let a1,a2 = $2 in Unit(a1, a2)         }

  | CONSTRUCTOR    attri_params { let a1,a2 = $2 in Constructor(a1, a2)  }
  | INJECTIVE      attri_params { let a1,a2 = $2 in Injective(a1, a2)    }
  | PREDICATE      attri_params { let a1,a2 = $2 in Predicate(a1, a2)    }

  | FUNCTIONAL     attri_params { let a1,a2 = $2 in Functional(a1, a2)   }
  | FUNCTION       attri_params { let a1,a2 = $2 in Function(a1, a2)     }

  | ANYWHERE       attri_params { let a1,a2 = $2 in Anywhere(a1, a2)     }
  | OWISE          attri_params { let a1,a2 = $2 in Owise(a1, a2)        }

  | SUBSORT        attri_params { let a1,a2 = $2 in Subsort(a1, a2)      }
  | PROJECTION     attri_params { let a1,a2 = $2 in Projection(a1, a2)   }
  | INITIALIZER    attri_params { let a1,a2 = $2 in Initializer(a1, a2)  }

  | name           attri_params { Format.printf (Common.Color.yel "WARNING: Attribute named %s is new!\n") $1;
                                  let a1,a2 = $2 in Other($1, (a1, a2))  }

core_attributes:
  | attribute                               {  [$1]  }
  | attribute COMMA core_attributes         { $1::$3 }
  | useless_attribute                       {   []   }
  | useless_attribute COMMA core_attributes {   $3   }

attributes:
  //| L_SQUARE_BRA separated_list(COMMA, attribute) R_SQUARE_BRA { $2 }
  | L_SQUARE_BRA                 R_SQUARE_BRA { [] }
  | L_SQUARE_BRA core_attributes R_SQUARE_BRA { $2 }

file:
  | axiom               { F_exec $1 }
  | attributes kmodules { F_sem($1, $2) }
  | EOF                 { F_sem([], []) }
