Mill is the preferred means of building

# How to run the test bench
mill raytracer.test.testOnly raytracer_datapath.Datapath_test
sbt test:testOnly raytracer_datapath.Datapath_test

mill raytracer.test
sbt test

# current problems
- Datapath_test.scala cannot go through all tests in a single run because the
verilator model for two different DUTs have a hash collision
- Datapath_test.scala creates a separate verilator model for each test,
  which negates the point of caching.
- EmitVerilog fails with "Exception in thread "main" circt.stage.phases.Exceptions$FirtoolNonZeroExitCode: firtool returned a non-zero exit code. Note that this version of Chisel (5.0.0) was published against firtool version 1.40.0."