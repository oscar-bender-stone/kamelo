module DICO
    imports INT
    imports BOOL

    syntax Bool ::= KItem "==Dico" KItem [function]

// ### Dico unit

  syntax Dico ::= ".Dico"                                [constructor, functional, symbol]
  syntax Dico ::= "(" KItem "|-->" KItem ")" "," Dico    [constructor, functional, symbol]


// ### Dico lookup (assume X is in the dictonary)

  syntax KItem ::= "lookup" KItem Dico [function]

  rule lookup X ((Y |--> _) , D) => lookup X D requires notBool(X ==Dico Y)
  rule lookup X ((Y |--> I) , _) => I          requires X ==Dico Y //[owise]


// ### Dico update (assume X is in the dictonary)

  syntax Dico ::= "update" KItem KItem Dico [function]

  rule update X V ((Y |--> W) , D) => ((Y |--> W) , (update X V D)) requires notBool(X ==Dico Y)
  rule update X V ((Y |--> _) , D) => ((Y |--> V) , D)              requires X ==Dico Y //[owise]


// ### Dico key membership

  syntax Bool ::= KItem "in_keys_dico" "(" Dico ")"       [function, functional]

  rule X in_keys_dico ((Y |--> _) , D) => X in_keys_dico(D) requires notBool(X ==Dico Y)
  rule X in_keys_dico ((Y |--> _) , _) => true              requires X ==Dico Y // [owise]
  rule _ in_keys_dico (.Dico) => false

endmodule

module IMP-DICO-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX

    syntax Iden ::= "var" "(" Int ")"

    syntax Bool ::= Iden "==Var" Iden [function, functional]

    syntax AExp ::= Iden | Int
                  | "(" AExp ")"      [bracket]
                  | AExp "+" AExp     [left, strict]
                  | AExp "-" AExp     [left, strict]
                  > AExp "/" AExp     [left, strict]
                  | AExp "*" AExp     [left, strict]

    syntax BExp ::= Bool
                  | "(" BExp ")"      [bracket]
                  | AExp "<=" AExp    [strict, latex({#1}\leq{#2})]
                  | AExp "<"  AExp    [strict]

                  | "not" BExp        [strict]
                  > BExp "and" BExp   [left, strict(1)]

    syntax Stmt ::= "{" "}"
                  | "{" Stmt "}"
                  | Iden "=" AExp ";"   [strict(2)]
                  | "if" BExp 
                    "then" Stmt 
                    "else" Stmt       [strict(1)]
                  | "while" BExp "do" Stmt
                  > Stmt Stmt         [left]

    syntax Pgm ::= "decl" IdenL ";" Stmt
    syntax IdenL ::= List{Iden,","}


    syntax KResult ::= Int | Bool

endmodule

module IMP-DICO
    imports INT
    imports BOOL
    imports DICO
    imports IMP-DICO-SYNTAX

    configuration <T color="yellow">
                     <k color="green"> $PGM:Pgm </k>
                     <env color="red"> .Dico </env>
                  </T>

    rule X ==Dico Y => X ==Var Y
    rule var(N1) ==Var var(N2) => N1 ==Int N2

 // -----------------  AExp  ------------------------------
    rule <k> var(X) => lookup var(X) M ... </k> <env> M </env>
    rule I1 + I2 => I1 +Int I2
    rule I1 - I2 => I1 -Int I2
    rule I1 / I2 => I1 /Int I2 requires I2 =/=Int 0
    rule I1 * I2 => I1 *Int I2

 // -----------------  BExp  -------------------------------
    rule I1 <= I2 => I1  <=Int I2
    rule I1  < I2 => I1   <Int I2

    rule not B => notBool B
    rule true  and B => B
    rule false and _ => false

 // -----------------  Stmt  -------------------------------
    rule {   } => . 
    rule { S } => S    

    rule <k> var(X) = I:Int; => . ... </k>
         <env> D => update var(X) I D </env>

    rule S1:Stmt S2:Stmt => S1 ~> S2

    rule if true  then S1 else _  => S1
    rule if false then _  else S2 => S2

    rule while B do S => if B then { S (while B do S) } else { }

 // -----------------  Pgm  ---------------------------------
    rule <k> decl (var(X),Xs => Xs);_ </k>
         <env> D:Dico => ((var(X) |--> 0) , D) </env> requires notBool (var(X) in_keys_dico(D))
    rule decl .IdenL; S => S

endmodule
