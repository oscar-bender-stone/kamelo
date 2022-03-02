
module AND-RULES-SYNTAX
  import BOOL
<<<<<<< HEAD
    syntax BExp ::= Bool
                   | "(" BExp ")"
                   | BExp "and" BExp  [left]
=======
  syntax BExp ::= Bool
                | "(" BExp ")"
                | BExp "and" BExp  [left]
>>>>>>> ci_new_bis
endmodule

module AND-RULES
  imports AND-RULES-SYNTAX
  syntax KResult ::= Bool

  configuration <T> <k> $PGM:BExp </k> </T>

  syntax KItem ::= freezer(BExp)

  // rule HOLE:BExp and BE2:BExp     => HOLE ~> HOLE and BE2 requires isKResult(E)  [heat]
  // rule BE1:BExp ~>  HOLE and BE2 => BE1 and BE2              [cool]


  rule <k> HOLE:BExp and BE:BExp => HOLE ~> freezer(BE) ...</k> [heat]
  rule <k> HOLE:BExp ~> freezer(E) => HOLE and E ...</k> requires isKResult(HOLE) [cool]


  rule true  and B => B
  rule false and _ => false

endmodule
