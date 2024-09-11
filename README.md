
# Documentation
The Ray Tracer Datapath is documented in our arXiv article: [A
Hardware Ray Tracer Datapath with Generalized
Features](https://arxiv.org/abs/2409.06000) where the following are discussed:
- Supported operations
- IO specification
- Configurable parameters
- Dataflow of pipeline stages
- Hardware assets
- Design choices
- Description of source files

# Prerequisites
The code in this project was tested with verilator version 4.038 and sbt version 1.8 or mill version 0.10.12.
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
