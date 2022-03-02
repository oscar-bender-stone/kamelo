
module AND-ATTRIBUT-SYNTAX
  import BOOL
  syntax BExp ::= Bool
                  | "(" BExp ")" [bracket]
                  | BExp "and" BExp  [left, strict(1)]

endmodule

module AND-ATTRIBUT
  imports AND-ATTRIBUT-SYNTAX
  syntax KResult ::= Bool

  configuration <T> <k> $PGM:BExp </k> </T>

  rule true  and B => B
  rule false and _ => false

endmodule
