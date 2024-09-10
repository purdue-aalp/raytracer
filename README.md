The code in this project was tested with verilator version 4.038 and sbt version 1.8 or mill version 0.10.12.

# Prerequisites
- verilator (for running the testbenches)
- sbt (optional) (for building the project with SBT)

# How to run the test bench
## Test top module only.
```
mill raytracer.test.testOnly raytracer_datapath.Datapath_test
``` 
or 
```
sbt test:testOnly raytracer_datapath.Datapath_test
```

Mill should be ready to use because a wrapper file for mill 0.10.12 is included with
this project. 

## Test top module as well as individual components.
```
mill raytracer.test
``` 
or 
```
sbt test
```
