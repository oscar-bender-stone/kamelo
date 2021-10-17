module MAX-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX

syntax Int ::= Int "max" Int [function]

endmodule


module MAX
    imports MAX-SYNTAX
    imports INT
    imports BOOL

    rule X max Y => Y requires X <Int Y
    rule X max Y => X requires X >=Int Y


endmodule
