%{
    open Common.Type
%}

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

%token LOCATION
%token SOURCE
%token PRODUCTION

%token <string> IDENT       // Identifiant
%token <string> STRING
%token EOF                  // End of file

%start exec

%type <Common.Type.name> name
%type <Common.Type.quant_var> quant_var
%type <Common.Type.param> param
%type <Common.Type.sort> sort

%type <Common.Type.symbol> symbol
%type <Common.Type.axiom> axiom

%type <Common.Type.axiom> exec
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
  | predicate           { Predicat $1 }

instance_symbol:
  | name L_CURLY_BRA R_CURLY_BRA L_PAREN R_PAREN { $1 }
  | STRING { $1 }
  //|        { "" }

attri_params:
  | L_CURLY_BRA param_list R_CURLY_BRA L_PAREN separated_list(COMMA, instance_symbol) R_PAREN
      { ($2, $5) }

exec:
  | axiom { $1 }
