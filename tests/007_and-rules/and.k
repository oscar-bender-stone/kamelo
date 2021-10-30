
module AND-SYNTAX
  import BOOL
  syntax BExp ::= Bool | BExp "and" BExp  [left]
endmodule

module AND
  imports AND-SYNTAX
  syntax KResult ::= Bool

  configuration <T> <k> $PGM:BExp </k> </T>

  rule HOLE:BExp and BE2:BExp   => HOLE ~>  [] and BE2 ... [heat]
  rule HOLE:BExp ~>  [] and BE2 => HOLE and BE2 ...        [cool]

  rule true  and B => B
  rule false and _ => false

endmodule
