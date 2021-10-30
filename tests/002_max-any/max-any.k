module MAX-ANY-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX

    syntax Exp ::= Int | "(" Exp ")" [bracket]
                | "max" Exp Exp // Marche aussi [functional]

endmodule


module MAX-ANY
    imports MAX-ANY-SYNTAX
    imports INT
    imports BOOL

    rule max X Y => Y requires X <Int  Y  [anywhere]
    rule max X Y => X requires X >=Int Y  [anywhere]


endmodule
