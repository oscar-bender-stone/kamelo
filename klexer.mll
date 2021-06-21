{
  type token =
  | EOF
  | IDENT of string
  | STRING of string

  | MODULE
  | ENDMODULE
  | IMPORT

  | SORT
  | H_SORT
  | SYMBOL
  | H_SYMBOL
  | ALIAS
  | WHERE
  | AXIOM

  | L_CURLY_BRA
  | R_CURLY_BRA
  | L_SQUARE_BRA
  | R_SQUARE_BRA
  | L_PAREN
  | R_PAREN
  | COLON
  | COMMA
  | DEF

  | EQUALS
  | EXISTS
  | AND
  | OR
  | NOT
  | IMPLIES
  | BOTTOM
  | TOP
  | REWRITES
  | IN
  | DOM_VAL

  | ASSOC
  | COMM
  | IDEM
  | UNIT

  | STRICT
  | SEQSTRICT

  | COOL
  | COOLLIKE
  | HEAT

  | SIMPLIFICATION

  | LEFT
  | RIGHT
  | PRIORITIES

  | CONSTRUCTOR
  | INJECTIVE
  | PREDICATE

  | FUNCTIONAL
  | FUNCTION

  | OWISE

  | SUBSORT
  | PROJECTION
  | INITIALIZER

  | TOPCELLINIT
  | TOPCELL
  | CELL
  | MAINCELL
  | CELLNAME
  | CELLFRAGMENT
  | CELLOPTABST

  | EQEQK
  | NOTEQEQK
  | BOOLOP
  | USERLIST

  | IMPURE
  | CONCRETE

  | LATEX
  | COLOR
  | COLORS
  | PREFER
  | NOTHREAD
  | HOOK

  | SMTLIB
  | SMTHOOK
  | FORMAT

  | STARTLINE
  | STARTCOLUMN

  | TOKEN
  | KLABEL
  | TERMINALS
  | INDEX
  | FRESHGENERATOR

  | KEYWORD
  | UNIQUE
  | LOCATION
  | SOURCE
  | PRODUCTION

  | ELEMENT
  | CONCAT

  | SORTINJECT
  | HASDOMAINVAL

(** [locate loc] converts the pair of position [loc] of the Lexing library
    into a quadruplet (start_line, start_col, end_line, end_col). *)
let locate : Lexing.position * Lexing.position -> int * int * int * int =
  fun (p1, p2) ->
  let start_line = p1.pos_lnum in
  let start_col = p1.pos_cnum - p1.pos_bol in
  let end_line = p2.pos_lnum in
  let end_col = p2.pos_cnum - p2.pos_bol in
  (start_line, start_col, end_line, end_col)

let unexpected_char : Lexing.lexbuf -> char -> token = fun lexbuf c ->
  let sl, sc, el, ec = locate (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
  Pos.fatal None
     "Unexpected characters [%c] between %i:%i to %i:%i." c sl sc el ec

exception SyntaxError

exception EOFError of string

}

let letter = ['a'-'z' 'A'-'Z' '_' '-' '@' '\'']+
let nb     = ['0'-'9']+
let ident  = ['a'-'z' 'A'-'Z' '_' '-' '@' '\'' '0'-'9']+

let location   = "org"['a'-'z' 'A'-'Z' '_' '-' '\'' '0'-'9']+ ("Location")
let source     = "org"['a'-'z' 'A'-'Z' '_' '-' '\'' '0'-'9']+ ("Source")
let production = "org"['a'-'z' 'A'-'Z' '_' '-' '\'' '0'-'9']+ ("Production")

rule token = parse
  | eof                  { EOF            }
  | [' ' '\t' '\n']      { token lexbuf   }    (* skip blanks *)
  | "//"                 { comment lexbuf }

  | "module"             { MODULE         }
  | "endmodule"          { ENDMODULE      }
  | "import"             { IMPORT         }

  | "sort"               { SORT           }
  | "hooked-sort"        { H_SORT         }
  | "symbol"             { SYMBOL         }
  | "hooked-symbol"      { H_SYMBOL       }
  | "alias"              { ALIAS          }
  | "where"              { WHERE          }
  | "axiom"              { AXIOM          }

  | '{'                  { L_CURLY_BRA    }
  | '}'                  { R_CURLY_BRA    }
  | '['                  { L_SQUARE_BRA   }
  | ']'                  { R_SQUARE_BRA   }
  | '('                  { L_PAREN        }
  | ')'                  { R_PAREN        }
  | ':'                  { COLON          }
  | ','                  { COMMA          }
  | ":="                 { DEF            }

  | "\\equals"           { EQUALS         }
  | "\\exists"           { EXISTS         }
  | "\\and"              { AND            }
  | "\\or"               { OR             }
  | "\\not"              { NOT            }
  | "\\implies"          { IMPLIES        }
  | "\\bottom"           { BOTTOM         }
  | "\\top"              { TOP            }
  | "\\rewrites"         { REWRITES       }
  | "\\in"               { IN             }
  | "\\dv"               { DOM_VAL        }

  | "assoc"              { ASSOC          }
  | "comm"               { COMM           }
  | "idem"               { IDEM           }
  | "unit"               { UNIT           }

  | "strict"             { STRICT         }
  | "seqstrict"          { SEQSTRICT      }

  | "cool"               { COOL           }
  | "cool-like"          { COOLLIKE       }
  | "heat"               { HEAT           }

  | "simplification"     { SIMPLIFICATION }

  | "left"               { LEFT           }
  | "right"              { RIGHT          }
  | "priorities"         { PRIORITIES     }

  | "constructor"        { CONSTRUCTOR    }
  | "injective"          { INJECTIVE      }
  | "predicate"          { PREDICATE      }

  | "functional"         { FUNCTIONAL     }
  | "function"           { FUNCTION       }

  | "owise"              { OWISE          }

  | "subsort"            { SUBSORT        }
  | "projection"         { PROJECTION     }
  | "initializer"        { INITIALIZER    }

  | "topCellInitializer" { TOPCELLINIT    }
  | "topcell"            { TOPCELL        }
  | "cell"               { CELL           }
  | "maincell"           { MAINCELL       }
  | "cellName"           { CELLNAME       }
  | "cellFragment"       { CELLFRAGMENT   }
  | "cellOptAbsent"      { CELLOPTABST    }

  | "equalEqualK"        { EQEQK          }
  | "notEqualEqualK"     { NOTEQEQK       }
  | "boolOperation"      { BOOLOP         }
  | "userList"           { USERLIST       }

  | "impure"             { IMPURE         }
  | "concrete"           { CONCRETE       }

  | "latex"              { LATEX          }
  | "color"              { COLOR          }
  | "colors"             { COLORS         }
  | "prefer"             { PREFER         }
  | "noThread"           { NOTHREAD       }
  | "hook"               { HOOK           }

  | "smtlib"             { SMTLIB         }
  | "smt-hook"           { SMTHOOK        }
  | "format"             { FORMAT         }

  | "contentStartLine"   { STARTLINE      }
  | "contentStartColumn" { STARTCOLUMN    }

  | "token"              { TOKEN          }
  | "klabel"             { KLABEL         }
  | "terminals"          { TERMINALS      }
  | "index"              { INDEX          }
  | "freshGenerator"     { FRESHGENERATOR }

  | "symbol'Kywd'"       { KEYWORD        }
  | "UNIQUE'Unds'ID"     { UNIQUE         }

  | location             { LOCATION       }
  | source               { SOURCE         }
  | production           { PRODUCTION     }

  | "element"            { ELEMENT        }
  | "concat"             { CONCAT         }

  | "sortInjection"      { SORTINJECT     }
  | "hasDomainValues"    { HASDOMAINVAL   }

  | '"'             { quote (Buffer.create 200) lexbuf }
  | ident           { let yytext = Lexing.lexeme lexbuf in
		              IDENT yytext }   (* WARNING The first case is considered *)
  | _ as c          { unexpected_char lexbuf c  }

and comment = parse
  | '\n' { token   lexbuf                                          }
  | _    { comment lexbuf                                          }
  | eof  { raise (EOFError "Unexpected end of file in comment.")   }

and quote buf = parse
  | '\\'    { after_backslash buf lexbuf                           }
  | '"'     { STRING (Buffer.contents buf)                         }
  | _ as c  { Buffer.add_char buf c; quote buf lexbuf              }
  | eof     { raise (EOFError "Unexpected end of file in string.") }

and after_backslash buf = parse
  | '"'    { quote buf lexbuf }
  | _ as c { Buffer.add_char buf '\\'; Buffer.add_char buf c; quote buf lexbuf }
