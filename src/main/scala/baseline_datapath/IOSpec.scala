package baseline_datapath

import chisel3._

class Float3 extends Bundle {
  /**
    * Defines a point in three-dimensional space.
    * All values are IEEE-754 floats (32 bits).
    */
  val x = Bits(32.W)
  val y = Bits(32.W)
  val z = Bits(32.W)
}

class Ray extends Bundle {
  /**
    * A ray in three-dimensional space.
    */
  val origin = new Float3()
  val dir = new Float3()
  val inv = new Float3()
  val extent = Bits(32.W)
}

class AABB extends Bundle {
  /**
    * Defines an Axis-Aligned Bounding Box (AABB) in three-dimensional space.
    * All values are IEEE-754 floats (32 bits) .
    */
  val x_min = Bits(32.W)
  val y_min = Bits(32.W)
  val z_min = Bits(32.W)
  
  val x_max = Bits(32.W)
  val y_max = Bits(32.W)
  val z_max = Bits(32.W)
}
