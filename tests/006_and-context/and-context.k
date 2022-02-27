
module AND-CONTEXT-SYNTAX
  import BOOL
  syntax BExp ::= Bool
                | "(" BExp ")" [bracket]
                | BExp "and" BExp  [left]
endmodule

module AND-CONTEXT
  imports AND-CONTEXT-SYNTAX
  syntax KResult ::= Bool

  configuration <T> <k> $PGM:BExp </k> </T>

  context HOLE and _

  rule true  and B => B
  rule false and _ => false

endmodule
