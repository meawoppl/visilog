// Rough order of operations for this module.
// 1. Parse the specified verilog "top" module.
// 2. Validate the module, try to provide useful debugging information.
// 3. Recrsively descend all submodules, and gather all register definitions
// 4. Create a StateStore with all the register definitions.
// 5. Descend all the assign statements, and create the expression graph
// 6. Compute all the pos/neg edge statements
// 7. Create the event queue for actual simulation state managment
// 8. Queue all the initial and unconditiona always blocks
// 9. Run the simulation
