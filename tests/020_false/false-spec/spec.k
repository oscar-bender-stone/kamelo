requires "false.k"

module VERIFICATION
    imports FALSE
endmodule

module SPEC
    imports VERIFICATION

    claim p _ => OK

endmodule
