package baseline_datapath

import chisel3._
import hardfloat._

class Datapath extends Module {
  val ray = IO(Input(new Ray(recorded_float = false)))
  val aabb = IO(Input(new AABB(recorded_float = false)))
  val sum = IO(Output(Bits(32.W)))

  //
  // STAGE 1: REGISTER INPUTS
  //
  val registered_ray = RegNext(ray)
  val registered_aabb = RegNext(aabb)

  //
  // STAGE 2: CONVERT FLOAT FORMAT: 32->33
  //
  val ray_rec_next = RayConvertFNtoRecFN(registered_ray)
  val aabb_rec_next = AABBConvertFNtoRecFN(registered_aabb)
  val ray_rec = RegNext(ray_rec_next)
  val aabb_rec = RegNext(aabb_rec_next)

  //
  // STAGE 3: ADD
  //
  val flattened_values = VecInit(Seq(
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
  ))

  // RecFN's exponent has an additional bit
  val recFNSum = Reg(Bits(33.W))

  recFNSum := flattened_values.tail.foldLeft(flattened_values.head){(x,y) => 
      // RecFN's exponent has an additional bit
      val fu = Module(new AddRecFN(8+1, 24))
      fu.io.subOp := false.B
      fu.io.a := x
      fu.io.b := y 
      fu.io.roundingMode := consts.round_near_even
      fu.io.detectTininess := consts.tininess_afterRounding 
      fu.io.out 
  }

  //
  // STAGE 4: CONVERT FLOAT FORMAT: 33->32
  //
  // Here's a quirk about the API: although recFNSum has 9 bits of exponent, the
  // "expWidth" function argument of fNFromRecFN actually reflects that of the
  // output value. Hence the argument is 8 not 9. 
  sum := fNFromRecFN(8, 24, recFNSum)
}
