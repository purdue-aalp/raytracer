package baseline_datapath

import chisel3._
import hardfloat._

class Datapath extends Module {
  val ray = IO(Input(new Ray(recorded_float = false)))
  val aabb = IO(Input(new AABB(recorded_float = false)))
  val sum = IO(Output(Bits(32.W)))

  // convert ieee float input to recorded float
  val ray_rec = RayConvertFNtoRecFN(ray)
  val aabb_rec = AABBConvertFNtoRecFN(aabb)

  val flattened_inputs = VecInit(
    Seq(
      ray_rec.dir.x,
      ray_rec.dir.y,
      ray_rec.dir.z,
      ray_rec.origin.x,
      ray_rec.origin.y,
      ray_rec.origin.z,
      ray_rec.inv.x,
      ray_rec.inv.y,
      ray_rec.inv.z,
      ray_rec.extent,
      aabb_rec.x_max,
      aabb_rec.x_min,
      aabb_rec.y_max,
      aabb_rec.y_min,
      aabb_rec.z_max,
      aabb_rec.z_min
    )
  )

  // RecFN's exponent has an additional bit
  val recFNSum = Wire(Bits(33.W))

  recFNSum := flattened_inputs.tail.foldLeft(flattened_inputs.head){(x,y) => 
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
