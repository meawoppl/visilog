module complex_test;
  reg clk, reset;
  reg [3:0] a, b;
  wire [3:0] sum;
  wire carry_out;

  // Instantiate the adder module
  adder uut (
    .a(a),
    .b(b),
    .sum(sum),
    .carry_out(carry_out)
  );

  // Clock generation
  always #5 clk = ~clk;

  initial begin
    // Initialize signals
    clk = 0;
    reset = 1;
    a = 0;
    b = 0;
    #10;
    reset = 0;

    // Apply test vectors
    a = 4'b0001; b = 4'b0010;
    #10;
    a = 4'b0101; b = 4'b0110;
    #10;
    a = 4'b1111; b = 4'b0001;
    #10;
    a = 4'b1010; b = 4'b0101;
    #10;
  end

  initial begin
    $monitor("At time %t, a = %b, b = %b, sum = %b, carry_out = %b", $time, a, b, sum, carry_out);
  end
endmodule

module adder (
  input [3:0] a,
  input [3:0] b,
  output [3:0] sum,
  output carry_out
);
  assign {carry_out, sum} = a + b;
endmodule
