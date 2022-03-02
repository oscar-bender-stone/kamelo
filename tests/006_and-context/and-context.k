
module AND-CONTEXT-SYNTAX
  import BOOL
  syntax BExp ::= Bool
<<<<<<< HEAD
                  | "(" BExp ")" [bracket]
                  | BExp "and" BExp  [left]
=======
                | "(" BExp ")" [bracket]
                | BExp "and" BExp  [left]
>>>>>>> ci_new_bis
endmodule

module AND-CONTEXT
  imports AND-CONTEXT-SYNTAX
  syntax KResult ::= Bool

  configuration <T> <k> $PGM:BExp </k> </T>

  context HOLE and _

  rule true  and B => B
  rule false and _ => false

endmodule
