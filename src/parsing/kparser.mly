%{
    open Common.Type
    open Common.Error
    open Count_line

    let claim_used = ref false

    let expand_binary_left_assoc : axiom -> axiom = fun p ->
      let rec aux = function
        | Predicate(Sym(name, p_l, arg_l)) ->
           (match arg_l with
            | [] -> raise (KaMeLoError (InternalError, "Kparser", "expand_binary_left_assoc", "This function needs to be fixed."))
            | [v] -> v
            | [_;_] -> Predicate(Sym(name, p_l, arg_l))
            | v1::v2::v3::q ->
               Predicate(Sym(name, p_l,
                  [v1;(aux (Predicate(Sym(name, p_l, (v2::v3::q)))))])))
        | _ -> raise (KaMeLoError (InternalError, "Kparser", "expand_binary_left_assoc", "This function needs to be fixed."))

      in
      aux p

    (* [get_source_line s] returns the line in the source file
       where the command is declared.
       Note that the argument of the attribute Location is
        "Location(2186,26,2186,121)" *)
    let get_source_line s : int option = match s with
        | Some s ->
           (* The index of the first occurrence of ',' in s after position 0. *)
           let pos = String.index_from s 0 ',' in
           Some (int_of_string(String.sub s 9 (pos - 9)))
        | None -> None

    (* [get_source_filename s] returns the filename of the source file.
       Note that the argument of the attribute Source is
        "Source(/usr/include/kframework/builtin/domains.md)" *)
    let get_source_filename s : string option = match s with
        | Some s ->
           (* The index of the last occurrence of '/' in s before the end of s. *)
           let pos = String.rindex_from s ((String.length s) - 1) '/' in
           Some (String.sub s (pos + 1) ((String.length s) - 1 - (pos + 1)))
        | None -> None

    let create_kommand_data l =
      let rec aux id location filename = function
        | [] -> (id, get_source_line location, get_source_filename filename)
        | Unique(_, [a2])::q   -> aux (Some a2) location  filename  q
        | Location(_, [a2])::q -> aux    id     (Some a2) filename  q
        | Source(_, [a2])::q   -> aux    id     location  (Some a2) q
        | _ -> failwith "Update create_kommand_data"
      in
      aux None None None l

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
%token CLAIM

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
%token CEIL

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

%token LEFT_ASSOC

%token LEFT
%token RIGHT
%token PRIORITY
%token PRIORITIES

%token PRIVATE

%token CONSTRUCTOR
%token INJECTIVE
%token PREDICATE

%token TOTAL
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
%type <bool * Common.Type.attribute> attribute
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
  | name L_CURLY_BRA quant_var_list R_CURLY_BRA
         L_PAREN param_list R_PAREN COLON param { ($1, $3, $6, $9) }

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
  | L_CURLY_BRA param_list R_CURLY_BRA
    L_PAREN axiom COMMA axiom R_PAREN { ($2, $5, $7) }

op_quant:
  | L_CURLY_BRA param_list R_CURLY_BRA
    L_PAREN name COLON param COMMA axiom R_PAREN { ($2, ($5, $7) , $9) }

predicate:
  | name L_CURLY_BRA param_list R_CURLY_BRA
         L_PAREN separated_list(COMMA, axiom) R_PAREN { Sym ($1, $3, $6) }
  | name COLON param                                  { Var ($1, $3)     }

axiom:
  | EQUALS   op_bin   { let a1,a2,a3 = $2 in Equals(a1, a2, a3)   }
  | EXISTS   op_quant { let pl, q, a = $2 in Exists (pl, q, a)    }
  | AND      op_bin   { let a1,a2,a3 = $2 in And(a1, a2, a3)      }
  | OR       op_bin   { let a1,a2,a3 = $2 in Or(a1, a2, a3)       }
  | NOT      op_una   { let a1, a2   = $2 in Not(a1, a2)          }
  | IMPLIES  op_bin   { let a1,a2,a3 = $2 in Implies(a1, a2, a3)  }
  | BOTTOM   op_const { Bottom $2                                 }
  | TOP      op_const { Top $2                                    }
  | REWRITES op_bin   { let a1,a2,a3 = $2 in Rewrites(a1, a2, a3) }
  | IN       op_quant { let pl, q, a = $2 in In (pl, q, a)        }
  | DOM_VAL  L_CURLY_BRA sort R_CURLY_BRA L_PAREN STRING R_PAREN
                      { Dom_val ($3, $6)                          }
  | LEFT_ASSOC L_CURLY_BRA R_CURLY_BRA L_PAREN predicate R_PAREN
                      { expand_binary_left_assoc (Predicate $5)   }
  | CEIL     op_una   { let a1, a2   = $2 in Ceil(a1, a2)         }
  //| name L_CURLY_BRA param_list R_CURLY_BRA L_PAREN separated_list(COMMA, axiom) R_PAREN
  //   { Sym ($1, $3, $6) }
  //| name COLON param { Var ($1, $3) }
  | predicate         { Predicate $1                              }

kommand:
  | SORT     sort        attributes   { let (l1, l2) = $3 in
                                        Sort     $2,    (l1, (update_line(), create_kommand_data l2)) }
  | H_SORT   sort        attributes   { let (l1, l2) = $3 in
                                        H_sort   $2,    (l1, (update_line(), create_kommand_data l2)) }
  | SYMBOL   symbol      attributes   { let (l1, l2) = $3 in
                                        Symbol   $2,    (l1, (update_line(), create_kommand_data l2)) }
  | H_SYMBOL symbol      attributes   { let (l1, l2) = $3 in
                                        H_symbol $2,    (l1, (update_line(), create_kommand_data l2)) }
  | ALIAS symbol WHERE def attributes { let (l1, l2) = $5 in
                                        Alias ($2, $4), (l1, (update_line(), create_kommand_data l2)) }
  | AXIOM L_CURLY_BRA quant_var_list R_CURLY_BRA axiom attributes
                                      { let (l1, l2) = $6 in
                                        Axiom ($3, $5), (l1, (update_line(), create_kommand_data l2)) }
  | CLAIM L_CURLY_BRA quant_var_list R_CURLY_BRA axiom attributes
                                      { claim_used := true ; let (l1, l2) = $6 in
                                        Claim ($3, $5), (l1, (update_line(), create_kommand_data l2)) }

import:
  | IMPORT name attributes { let (l,_) = $3 in ($2, l) }

kmodules:
  | MODULE name import* kommand* ENDMODULE attributes kmodules
      { let (l,_) = $6 in ($2, $3, $4, l) :: $7 }
  | EOF
      {           []           }

instance_symbol:
  | name L_CURLY_BRA R_CURLY_BRA L_PAREN R_PAREN { $1 }
  | name COLON param                             { $1 }
  | STRING                                       { $1 }
  //|        { "" }

attri_params:
  | L_CURLY_BRA param_list R_CURLY_BRA
    L_PAREN separated_list(COMMA, instance_symbol) R_PAREN { ($2, $5) }

useless_attribute:
  | STRICT         attri_params { } // let a1,a2 = $2 in Strict(a1, a2)       }
  | SEQSTRICT      attri_params { } // let a1,a2 = $2 in Seqstrict(a1, a2)    }

  | COOLLIKE       attri_params { } // let a1,a2 = $2 in CoolLike(a1, a2)     }
  | STRUCTURAL     attri_params { } // let a1,a2 = $2 in Structural(a1, a2)   }

  | SIMPLIFICATION attri_params { } // let a1,a2 = $2 in Simpl(a1, a2)        }


  | LEFT           attri_params { } // let a1,a2 = $2 in Left(a1, a2)         }
  | RIGHT          attri_params { } // let a1,a2 = $2 in Right(a1, a2)        }
  | PRIORITY       attri_params { }
  | PRIORITIES     attri_params { } // let a1,a2 = $2 in Priorities(a1, a2)   }

  | PRIVATE        attri_params { }

  | TOPCELLINIT    attri_params { } // { let a1,a2 = $2 in Topcellinit (a1, a2) }
  | TOPCELL        attri_params { } // { let a1,a2 = $2 in Topcell(a1, a2)      }
  | CELL           attri_params { } // { let a1,a2 = $2 in Cell(a1, a2)         }
  | MAINCELL       attri_params { } // { let a1,a2 = $2 in Maincell(a1, a2)     }
  | CELLNAME       attri_params { } // { let a1,a2 = $2 in Cellname(a1, a2)     }
  | CELLFRAGMENT   attri_params { } // { let a1,a2 = $2 in Cellfragment(a1, a2) }
  | CELLOPTABST    attri_params { } // { let a1,a2 = $2 in Celloptabst(a1, a2)  }

  | NOTEQEQK       attri_params { }
  | EQEQK          attri_params { }
  | BOOLOP         attri_params { }
  | USERLIST       attri_params { }

  | IMPURE         attri_params { }
  | CONCRETE       attri_params { }

  | LATEX          attri_params { } // { let a1,a2 = $2 in Latex(a1, a2)        }
  | COLOR          attri_params { } // { let a1,a2 = $2 in Color(a1, a2)        }
  | COLORS         attri_params { } // { let a1,a2 = $2 in Colors(a1, a2)       }
  | PREFER         attri_params { } // { let a1,a2 = $2 in Prefer(a1, a2)       }
  | NOTHREAD       attri_params { } // { let a1,a2 = $2 in Nothread(a1, a2)     }
  | HOOK           attri_params { } // { let a1,a2 = $2 in Hook(a1, a2)         }

  | SMTLIB         attri_params { } // { let a1,a2 = $2 in SMTlib(a1, a2)       }
  | SMTHOOK        attri_params { } // { let a1,a2 = $2 in SMThook(a1, a2)      }
  | FORMAT         attri_params { } // { let a1,a2 = $2 in Format(a1, a2)       }
  | MACRO          attri_params { }
  | RESULT         attri_params { }

  | EXIT           attri_params { }
  | AVOID          attri_params { }
  | RETURNSUNIT    attri_params { }

  | STARTLINE      attri_params { } // { let a1,a2 = $2 in StartLine(a1, a2)    }
  | STARTCOLUMN    attri_params { } // { let a1,a2 = $2 in StartCol(a1, a2)     }

  | TOKEN          attri_params { } // { let a1,a2 = $2 in Token(a1, a2)        }
  | KLABEL         attri_params { } // { let a1,a2 = $2 in Klabel(a1, a2)       }
  | TERMINALS      attri_params { } // { let a1,a2 = $2 in Terminals(a1, a2)    }
  | INDEX          attri_params { } // { let a1,a2 = $2 in Index(a1, a2)        }
  | FRESHGENERATOR attri_params { }

  | KEYWORD        attri_params { } // { let a1,a2 = $2 in Keyword(a1, a2)      }
  | PRODUCTION     attri_params { } // { let a1,a2 = $2 in Production(a1, a2)   }

  | ELEMENT        attri_params { } // { let a1,a2 = $2 in Element(a1, a2)      }
  | CONCAT         attri_params { } // { let a1,a2 = $2 in Concat(a1, a2)       }

  | SORTINJECT     attri_params { } // { let a1,a2 = $2 in Sortinject(a1, a2)   }
  | HASDOMAINVAL   attri_params { } // { let a1,a2 = $2 in Hasdomainval(a1, a2) }

// true  if the attribute is used in the main workflow of the translation
// false if the attribute is used only as additional information

attribute:
  | UNIQUE         attri_params { let a1,a2 = $2 in (false, Unique(a1, a2))      }
  | LOCATION       attri_params { let a1,a2 = $2 in (false, Location(a1, a2))    }
  | SOURCE         attri_params { let a1,a2 = $2 in (false, Source(a1, a2))      }


  | ASSOC          attri_params { let a1,a2 = $2 in (true, Assoc(a1, a2))        }
  | COMM           attri_params { let a1,a2 = $2 in (true, Comm(a1, a2))         }
  | IDEM           attri_params { let a1,a2 = $2 in (true, Idem(a1, a2))         }
  | UNIT           attri_params { let a1,a2 = $2 in (true, Unit(a1, a2))         }

  | CONSTRUCTOR    attri_params { let a1,a2 = $2 in (true, Constructor(a1, a2))  }
  | INJECTIVE      attri_params { let a1,a2 = $2 in (true, Injective(a1, a2))    }
  | PREDICATE      attri_params { let a1,a2 = $2 in (true, Predicate(a1, a2))    }

  | TOTAL          attri_params { let a1,a2 = $2 in (true, Total(a1, a2))        }
  | FUNCTION       attri_params { let a1,a2 = $2 in (true, Function(a1, a2))     }

  | COOL           attri_params { let a1,a2 = $2 in (true, Cool(a1, a2))         }
  | HEAT           attri_params { let a1,a2 = $2 in (true, Heat(a1, a2))         }

  | ANYWHERE       attri_params { let a1,a2 = $2 in (true, Anywhere(a1, a2))     }
  | OWISE          attri_params { let a1,a2 = $2 in (true, Owise(a1, a2))        }

  | SUBSORT        attri_params { let a1,a2 = $2 in (true, Subsort(a1, a2))      }
  | PROJECTION     attri_params { let a1,a2 = $2 in (true, Projection(a1, a2))   }
  | INITIALIZER    attri_params { let a1,a2 = $2 in (true, Initializer(a1, a2))  }

  | name           attri_params
      { wrn_1 _STDOUT "WARNING: Attribute named %s is new!" $1;
        let a1,a2 = $2 in (true, Other($1, (a1, a2)))                            } // TODO ok ?

core_attributes:
  | attribute                               { let (b, attr) = $1 in
                                              if b then [attr], [] else [], [attr] }
  | attribute COMMA core_attributes         { let (b, attr) = $1 in
                                              let attr_l1, attr_l2 = $3 in
                                              if b
                                              then attr::attr_l1, attr_l2
                                              else attr_l1, attr::attr_l2          }
  | useless_attribute                       {   [], []   }
  | useless_attribute COMMA core_attributes {     $3     }

attributes:
  //| L_SQUARE_BRA separated_list(COMMA, attribute) R_SQUARE_BRA { $2 }
  | L_SQUARE_BRA                 R_SQUARE_BRA { [], [] }
  | L_SQUARE_BRA core_attributes R_SQUARE_BRA {   $2   }

file:
  | axiom axiom EOF          { F_pgm($1, $2)            }
  | axiom EOF                { F_pgm($1, $1)            } // TODO fix
  | attributes kmodules  EOF { let (l,_) = $1 in
                               if !claim_used
                               then F_spec_pgm(l, $2)
                               else F_sem(l, $2)       }
  | EOF                      { F_sem ([], [])           }
