module spi_controller (
    input wire clk,
    input wire rst,
    input wire mosi,
    output wire miso,
    output wire sclk,
    output wire cs
);
    reg [1:0] state;
    reg [7:0] data;

    localparam IDLE = 2'b00;
    localparam TRANSFER = 2'b01;
    localparam DONE = 2'b10;

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            state <= IDLE;
            data <= 8'b0;
        end else begin
            case (state)
                IDLE: begin
                    if (cs == 0) begin
                        state <= TRANSFER;
                    end
                end
                TRANSFER: begin
                    if (cs == 1) begin
                        state <= DONE;
                    end else begin
                        data <= {data[6:0], mosi};
                    end
                end
                DONE: begin
                    state <= IDLE;
                end
            endcase
        end
    end

    assign miso = data[7];
    assign sclk = clk;
    assign cs = (state == IDLE) ? 1 : 0;
endmodule
