Mill is the preferred means of building

# How to run the test bench
mill raytracer.test.testOnly baseline_datapath.Datapath_test_for_vcd
sbt test:testOnly baseline_datapath.Datapath_test_for_vcd

# current problems
- Datapath_test.scala cannot go through all tests in a single run because the
verilator model for two different DUTs have a hash collision
- Datapath_tet_for_vcd.scala creates a separate verilator model for each test,
  which negates the point of caching.
- EmitVerilog fails with "Exception in thread "main" circt.stage.phases.Exceptions$FirtoolNonZeroExitCode: firtool returned a non-zero exit code. Note that this version of Chisel (5.0.0) was published against firtool version 1.40.0."