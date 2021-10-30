module MAX-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX

syntax Int ::= "(" Int ")" [bracket]
               | "max" Int Int [function]

endmodule


module MAX
    imports MAX-SYNTAX
    imports INT
    imports BOOL

    rule max X Y => Y requires X <Int Y
    rule max X Y => X requires X >=Int Y


endmodule
