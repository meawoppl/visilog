module basic_test;
  reg a, b, c;
  wire y;

  // Instantiate the AND gate
  and (y, a, b, c);

  initial begin
    // Apply test vectors
    a = 0; b = 0; c = 0;
    #10;
    a = 0; b = 0; c = 1;
    #10;
    a = 0; b = 1; c = 0;
    #10;
    a = 0; b = 1; c = 1;
    #10;
    a = 1; b = 0; c = 0;
    #10;
    a = 1; b = 0; c = 1;
    #10;
    a = 1; b = 1; c = 0;
    #10;
    a = 1; b = 1; c = 1;
    #10;
  end

  initial begin
    $monitor("At time %t, a = %b, b = %b, c = %b, y = %b", $time, a, b, c, y);
  end
endmodule
