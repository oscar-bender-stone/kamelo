
module AND-SYNTAX
  import BOOL
  syntax BExp ::= Bool | BExp "and" BExp  [left]
endmodule

module AND
  imports AND-SYNTAX
  syntax KResult ::= Bool

  configuration <T> <k> $PGM:BExp </k> </T>

  context HOLE and _

  rule true  and B => B
  rule false and _ => false

endmodule
