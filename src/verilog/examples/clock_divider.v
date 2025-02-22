module clock_divider (
    input wire clk,
    input wire rst,
    output reg divided_clk
);
    reg [31:0] counter;

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            counter <= 32'b0;
            divided_clk <= 1'b0;
        end else begin
            counter <= counter + 1;
            if (counter == 32'd50000000) begin
                counter <= 32'b0;
                divided_clk <= ~divided_clk;
            end
        end
    end
endmodule
