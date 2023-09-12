package baseline_datapath

import chisel3._ 
import hardfloat._

class RecFNCompareSelect {
  /**
    * Given two input Recorded-format Floats `a` and `b`, output either `c` or
    * `d` depending on the result of comparison:
    * output = (a >= b) ? c : d  
    */
  val io = IO(new Bundle{
    val a = Input(new Float3(recorded_float = true))
    val b = Input(new Float3(recorded_float = true))
    val c = Input(new Float3(recorded_float = true))
    val d = Input(new Float3(recorded_float = true))
    val out = Output(new Float3(recorded_float = true))
  })

  
  
}
