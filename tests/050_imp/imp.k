module IMP-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX
    imports ID

    syntax AExp ::= Id | Int
                  | "(" AExp ")"      [bracket]
                  | AExp "+" AExp     [left, strict]
                  | AExp "-" AExp     [left, strict]
                  > AExp "/" AExp     [left, strict]

    syntax BExp ::= Bool
                  | "(" BExp ")"      [bracket]
                  | AExp "<=" AExp    [seqstrict, latex({#1}\leq{#2})]
                  | AExp "<"  AExp    [seqstrict]

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

    syntax Pgm ::= "var" Ids ";" Stmt
    syntax Ids ::= List{Id,","}


    syntax KResult ::= Int | Bool

    syntax Id ::= "$a" [token]
                | "$b" [token]
                | "$c" [token]
                | "$x" [token]
                | "$y" [token]
                | "$z" [token]
                | "$s" [token]
                | "$n" [token]

endmodule

module IMP
    imports INT
    imports BOOL
    imports MAP
    imports IMP-SYNTAX

    configuration <T color="yellow">
                     <k color="green"> $PGM:Pgm </k>
                     <env color="red"> .Map </env>
                  </T>

 // -----------------  AExp  ------------------------------
    rule <k> X:Id => I ...</k> <env>... X |-> I ...</env>
    rule I1 + I2 => I1 +Int I2
    rule I1 - I2 => I1 -Int I2
    rule I1 / I2 => I1 /Int I2 requires I2 =/=Int 0

 // -----------------  BExp  -------------------------------
    rule I1 <= I2 => I1  <=Int I2
    rule I1  < I2 => I1   <Int I2

    rule not B => notBool B
    rule true  and B => B
    rule false and _ => false

 // -----------------  Stmt  -------------------------------
    rule {   } => . 
    rule { S } => S    

    rule <k> X = I:Int; => . ... </k>
         <env>... X |-> (_ => I) ...</env>

    rule S1:Stmt S2:Stmt => S1 ~> S2

    rule if true  then S1 else _  => S1
    rule if false then _  else S2 => S2

    rule while B do S => if B then { S (while B do S) } else { }

 // -----------------  Pgm  ---------------------------------
    rule <k> var (X,Xs => Xs);_ </k>
         <env> Rho:Map (.Map => X |-> 0) </env> requires notBool (X in_keys(Rho))
    rule var .Ids; S => S

endmodule
