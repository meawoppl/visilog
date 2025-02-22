module complex_module (
    input wire clk,
    input wire rst,
    input wire [3:0] a,
    input wire [3:0] b,
    output reg [3:0] sum,
    inout wire [3:0] data
);
    reg [3:0] temp;

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            sum <= 4'b0000;
            temp <= 4'b0000;
        end else begin
            temp <= a + b;
            sum <= temp;
        end
    end

    assign data = (sum > 4'b1000) ? sum : 4'bzzzz;
endmodule
