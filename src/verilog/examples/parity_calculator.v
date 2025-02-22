module parity_calculator (
    input wire [7:0] data,
    output wire parity
);
    assign parity = ^data;
endmodule
