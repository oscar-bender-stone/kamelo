module MAX-MECHANT-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX

syntax Int ::= "(" Int ")" [bracket]
                | "max" Int Int [function]

endmodule


module MAX-MECHANT
    imports MAX-MECHANT-SYNTAX
    imports INT
    imports BOOL

    rule max X Y => Y requires X <Int Y
    rule max X 0 => X requires X >=Int 0
    rule max X (Y +Int 1) => X requires X >Int Y

endmodule
