The code in this project was tested with sbt version 1.8 or mill version 0.10.12.

# How to run the test bench
Test top module only.
```
mill raytracer.test.testOnly raytracer_datapath.Datapath_test
sbt test:testOnly raytracer_datapath.Datapath_test
```

Test top module as well as individual components.
```
mill raytracer.test
sbt test
```