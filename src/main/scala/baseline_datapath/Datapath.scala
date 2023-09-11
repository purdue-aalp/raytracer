package baseline_datapath

import chisel3._
import hardfloat._

class Datapath extends Module {
  val ray = IO(Input(new Ray()))
  val aabb = IO(Input(new AABB()))
  val sum = IO(Output(Bits(32.W)))

  val flattened_inputs = VecInit(
    Seq(
      ray.dir.x,
      ray.dir.y,
      ray.dir.z,
      ray.origin.x,
      ray.origin.y,
      ray.origin.z,
      ray.inv.x,
      ray.inv.y,
      ray.inv.z,
      ray.extent,
      aabb.x_max,
      aabb.x_min,
      aabb.y_max,
      aabb.y_min,
      aabb.z_max,
      aabb.z_min
    )
  )

  val recFNInputs = flattened_inputs.map(recFNFromFN(8, 24, _))

  // RecFN's exponent has an additional bit
  val recFNSum = Wire(Bits(33.W))

  recFNSum := recFNInputs.tail.foldLeft(recFNInputs.head){(x,y) => 
      // RecFN's exponent has an additional bit
      val fu = Module(new AddRecFN(8+1, 24))
      fu.io.subOp := false.B
      fu.io.a := x
      fu.io.b := y 
      fu.io.roundingMode := consts.round_near_even
      fu.io.detectTininess := consts.tininess_afterRounding 
      fu.io.out 
  }

  sum := fNFromRecFN(8, 24, recFNSum)
}
