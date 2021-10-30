
module C-SYNTAX
    syntax C ::= "c" [klabel("c"), symbol]
endmodule

module C
    imports C-SYNTAX
    syntax KResult ::= C
endmodule
