
module AND-SYNTAX
  import BOOL
  syntax BExp ::= Bool | BExp "and" BExp  [left, strict(1)]
endmodule

module AND
  imports AND-SYNTAX
  syntax KResult ::= Bool

  configuration <T> <k> $PGM:BExp </k> </T>

  rule true  and B => B
  rule false and _ => false

endmodule
