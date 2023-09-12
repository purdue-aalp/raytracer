package baseline_datapath

import chisel3._
import hardfloat._

/**
  * The compare-and-swap unit is a special case of the RecFNCompareSelect module.
  */
class CompareAndSwapRecFN extends Module {
  val io = IO(new Bundle{
    val x_in = Input(Bits(33.W))
    val y_in = Input(Bits(33.W))

    val x_out = Output(Bits(33.W))
    val y_out = Output(Bits(33.W))
  })

  val compare_select_unit = Module(new RecFNCompareSelect(
    option = false, passthrough_type = Bits(33.W)
  ))

  compare_select_unit.io.a := io.x_in 
  compare_select_unit.io.c := io.x_in 

  compare_select_unit.io.b := io.y_in 
  compare_select_unit.io.d := io.y_in 

  io.x_out := compare_select_unit.io.c_out 
  io.y_out := compare_select_unit.io.d_out
}
