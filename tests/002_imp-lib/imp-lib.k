
module IMP-LIB-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX
    imports ID-SYNTAX

    syntax AExp ::= Id | Int
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
                  | Id "=" AExp ";"   [strict(2)]
                  | "if" BExp
                    "then" Stmt
                    "else" Stmt       [strict(1)]
                  | "while" BExp "do" Stmt
                  > Stmt Stmt         [left]

    syntax Pgm ::= "decl" IdenL ";" Stmt
    syntax IdenL ::= List{Id,","}


    syntax KResult ::= Int | Bool

endmodule

module IMP-LIB
    imports INT
    imports BOOL
    imports MAP
    imports IMP-LIB-SYNTAX

    configuration <T color="yellow">
                     <k color="green"> $PGM:Pgm </k>
                     <env color="red"> .Map </env>
                  </T>

 // -----------------  AExp  ------------------------------
    rule <k> X:Id => M[X] ... </k> <env> M </env>
    rule I1 + I2 => I1 +Int I2
    rule I1 - I2 => I1 -Int I2
    rule I1 / I2 => I1 /Int I2 requires I2 =/=Int 0
    rule I1 * I2 => I1 *Int I2

 // -----------------  BExp  -------------------------------
    rule I1 <= I2 => I1  <=Int I2
    rule I1  < I2 => I1   <Int I2

    rule not B       => notBool B
    rule true  and B => B
    rule false and _ => false

 // -----------------  Stmt  -------------------------------
    rule {   } => .
    rule { S } => S

    rule <k> X = I:Int; => . ... </k>
         <env> M => M[X <- I] </env>

    rule S1:Stmt S2:Stmt => S1 ~> S2

    rule if true  then S1 else _  => S1
    rule if false then _  else S2 => S2

    rule while B do S => if B then { S (while B do S) } else { }

 // -----------------  Pgm  ---------------------------------
    rule <k> decl (X,Xs => Xs);_ </k>
         <env> M:Map => ((X |-> 0) M) </env> requires notBool (X in_keys(M))
    rule decl .IdenL; S => S

endmodule
