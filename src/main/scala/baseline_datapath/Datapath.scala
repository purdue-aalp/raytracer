package baseline_datapath

import chisel3._
import hardfloat._

class Datapath extends Module {
  val ray = IO(Input(new Ray(recorded_float = false)))
  val aabb = IO(Input(new AABB(recorded_float = false)))
  val sum = IO(Output(Bits(32.W)))

  // SOME CONSTANTS
  val _rounding_rule = consts.round_near_even
  val _tininess_rule = consts.tininess_beforeRounding
  val _zero_RecFN = {
    val convert_zero_int_to_zero_rec_float = Module(new INToRecFN(32, 8, 24))
    convert_zero_int_to_zero_rec_float.io.signedIn := true.B 
    convert_zero_int_to_zero_rec_float.io.in := 0.S(32.W)
    convert_zero_int_to_zero_rec_float.io.roundingMode := _rounding_rule
    convert_zero_int_to_zero_rec_float.io.detectTininess := _tininess_rule 
    val out_val = convert_zero_int_to_zero_rec_float.io.out 
    // TODO: handle exception flags!
    out_val
  }

  //
  // STAGE 1: REGISTER INPUTS
  //
  val ray_1 = RegNext(ray)
  val aabb_1 = RegNext(aabb)

  //
  // STAGE 2: CONVERT FLOAT FORMAT: 32->33
  //
  val ray_rec_next = RayConvertFNtoRecFN(ray_1)
  val aabb_rec_next = AABBConvertFNtoRecFN(aabb_1)
  val ray_2 = RegNext(ray_rec_next)
  val aabb_2 = RegNext(aabb_rec_next)

  //
  // STAGE 3:  translate box relative to ray origin
  //
  val ray_3 = RegNext(ray_2)
  val aabb_3 = Reg(new AABB(recorded_float = true))

  // the following implements the C-code:
    /*
    child.x_min = child.x_min - ray.origin.x;
    child.y_min = child.y_min - ray.origin.y;
    child.z_min = child.z_min - ray.origin.z;
    child.x_max = child.x_max - ray.origin.x;
    child.y_max = child.y_max - ray.origin.y;
    child.z_max = child.z_max - ray.origin.z;
    */
  {
    val _dest = Seq(
      aabb_3.x_min, aabb_3.y_min, aabb_3.z_min,
      aabb_3.x_max, aabb_3.y_max, aabb_3.z_max
    )
    val _src1 = Seq(
      aabb_2.x_min, aabb_2.y_min, aabb_2.z_min,
      aabb_2.x_max, aabb_2.y_max, aabb_2.z_max
    )
    val _src2 = Seq(
      ray_2.origin.x, ray_2.origin.y, ray_2.origin.z,
      ray_2.origin.x, ray_2.origin.y, ray_2.origin.z
    )
    (_dest zip _src1 zip _src2) foreach { case ((_1, _2), _3) => 
      val fu = Module(new AddRecFN(8+1, 24))
      fu.io.subOp := true.B 
      fu.io.a := _2 
      fu.io.b := _3
      fu.io.roundingMode := _rounding_rule
      fu.io.detectTininess := _tininess_rule 
      _1 := fu.io.out
      // TODO: handle exception flags!
    }
  }

  //
  // STAGE 4: Time intersection interval calculations for each axis plane
  //
  val tp_min_4 = Reg(new Float3(recorded_float = true))
  val tp_max_4 = Reg(new Float3(recorded_float = true))
  val aabb_4 = RegNext(aabb_3) // aren't really used
  val ray_4 = RegNext(ray_3)  

  // the following implements the C-code
  /*
    float tp_min_x = child.x_min * ray.inv.x;
    float tp_min_y = child.y_min * ray.inv.y;
    float tp_min_z = child.z_min * ray.inv.z;
    float tp_max_x = child.x_max * ray.inv.x;
    float tp_max_y = child.y_max * ray.inv.y;
    float tp_max_z = child.z_max * ray.inv.z;
  */
  {
    val _dest = Seq(
      tp_min_4.x, tp_min_4.y, tp_min_4.z,
      tp_max_4.x, tp_max_4.y, tp_max_4.z
    )
    val _src1 = Seq(
      aabb_3.x_min, aabb_3.y_min, aabb_3.z_min,
      aabb_3.x_max, aabb_3.y_max, aabb_3.z_max      
    )
    val _src2 = Seq(
      ray_3.inv.x, ray_3.inv.y, ray_3.inv.z,
      ray_3.inv.x, ray_3.inv.y, ray_3.inv.z
    )
    (_dest zip _src1 zip _src2) foreach {case((_1, _2), _3) =>
      val fu = Module(new MulRecFN(8+1, 24))
      fu.io.a := _2 
      fu.io.b := _3 
      fu.io.roundingMode := _rounding_rule
      fu.io.detectTininess := _tininess_rule
      _1 := fu.io.out 
      // TODO: handle exception flags!
    }
  }

  //
  // STAGE 5: SORTING
  //
  val t_min = Wire(new Float3(recorded_float = true))
  val t_max = Wire(new Float3(recorded_float = true))
  
  def flip_intervals_if_dir_is_neg(
    c_out: Bits, d_out: Bits,
    a: Bits, b: Bits, c: Bits, d: Bits, 
    dont_flip_if_equal: Boolean // scala type boolean, not Chisel type!
  ) = {
    val fu = Module(new RecFNCompareSelect(option=dont_flip_if_equal, passthrough_type=Bits(33.W)))
    c_out := fu.io.c_out 
    d_out := fu.io.d_out 
    fu.io.a := a 
    fu.io.b := b 
    fu.io.c := c 
    fu.io.d := d
  }
  
  flip_intervals_if_dir_is_neg(
    t_min.x, t_max.x,
    ray_4.dir.x, _zero_RecFN, tp_min_4.x, tp_max_4.x, 
    true
  )
  flip_intervals_if_dir_is_neg(
    t_min.y, t_max.y, 
    ray_4.dir.y, _zero_RecFN, tp_min_4.y, tp_max_4.y, 
    true
  )
  flip_intervals_if_dir_is_neg(
    t_min.z, t_max.z, 
    ray_4.dir.z, _zero_RecFN, tp_min_4.z, tp_max_4.z, 
    true
  )


  {
  // //
  // // STAGE 3: ADD
  // //
  //   val flattened_values = VecInit(Seq(
  //     ray_rec.dir.x,
  //     ray_rec.dir.y,
  //     ray_rec.dir.z,
  //     ray_rec.origin.x,
  //     ray_rec.origin.y,
  //     ray_rec.origin.z,
  //     ray_rec.inv.x,
  //     ray_rec.inv.y,
  //     ray_rec.inv.z,
  //     ray_rec.extent,
  //     aabb_rec.x_max,
  //     aabb_rec.x_min,
  //     aabb_rec.y_max,
  //     aabb_rec.y_min,
  //     aabb_rec.z_max,
  //     aabb_rec.z_min
  //   ))

  //   // RecFN's exponent has an additional bit
  //   val recFNSum = Reg(Bits(33.W))

  //   recFNSum := flattened_values.tail.foldLeft(flattened_values.head){(x,y) => 
  //       // RecFN's exponent has an additional bit
  //       val fu = Module(new AddRecFN(8+1, 24))
  //       fu.io.subOp := false.B
  //       fu.io.a := x
  //       fu.io.b := y 
  //       fu.io.roundingMode := consts.round_near_even
  //       fu.io.detectTininess := consts.tininess_afterRounding 
  //       fu.io.out 
  //   }
  // 
  // 
  // //
  // // STAGE 6: CONVERT FLOAT FORMAT: 33->32
  // //
  // // Here's a quirk about the API: although recFNSum has 9 bits of exponent, the
  // // "expWidth" function argument of fNFromRecFN actually reflects that of the
  // // output value. Hence the argument is 8 not 9. 
  // sum := fNFromRecFN(8, 24, recFNSum)
  }

  sum := 0.U(32.W)
}
