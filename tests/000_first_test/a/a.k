
module A-SYNTAX
    syntax A ::= "a"
endmodule

module A
    imports A-SYNTAX
    syntax KResult ::= A
endmodule
