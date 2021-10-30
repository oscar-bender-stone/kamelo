
module AND-ATTRIBUT-SYNTAX
  syntax Boolean ::= "top" | "bot"
  syntax BExp ::= Boolean | BExp "and" BExp  [left, strict(1)]
endmodule

module AND-ATTRIBUT
  imports AND-ATTRIBUT-SYNTAX
  syntax KResult ::= Boolean

  configuration <T> <k> $PGM:BExp </k> </T>

  rule top  and B => B
  rule bot and _ => bot

endmodule
