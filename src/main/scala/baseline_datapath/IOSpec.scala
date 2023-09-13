package baseline_datapath

import chisel3._
import hardfloat.{recFNFromFN, fNFromRecFN}

class Float3 (recorded_float: Boolean = false) extends Bundle {
  /**
    * Defines a point in three-dimensional space.
    * With "recorded_float" being false, the expected format is IEEE-754 32-bit float
    * With "recorded_float" being true, the expected format is (1 bit sign + 9
    * bit exponent + 23 bit fraction). This is the 33-bit long "recorded float"
    * format used by the HardFloat library, that corresponds to the 32-bit
    * standard float.
    */
  val bit_width = if(recorded_float) 33.W else 32.W
  
  val x = Bits(bit_width)
  val y = Bits(bit_width)
  val z = Bits(bit_width)

  def isRecordedFloat(): Boolean = {recorded_float}
}

class Ray (recorded_float: Boolean = false) extends Bundle {
  /**
    * A ray in three-dimensional space.
    */
  val origin = new Float3(recorded_float)
  val dir = new Float3(recorded_float)
  val inv = new Float3(recorded_float)

  val _bit_width = if(recorded_float) 33.W else 32.W
  val extent = Bits(_bit_width)

  def isRecordedFloat(): Boolean = {recorded_float}
}

class AABB (recorded_float: Boolean = false) extends Bundle {
  /**
    * Defines an Axis-Aligned Bounding Box (AABB) in three-dimensional space.
    * All values are either IEEE-754 floats (!recorded_float) or 33-bit recorded
    * floats .
    */

  val bit_width = if(recorded_float) 33.W else 32.W

  val x_min = Bits(bit_width)
  val y_min = Bits(bit_width)
  val z_min = Bits(bit_width)
  
  val x_max = Bits(bit_width)
  val y_max = Bits(bit_width)
  val z_max = Bits(bit_width)

  def isRecordedFloat(): Boolean = {recorded_float} 
}


// Conversion circuits between 32-bit IEEE float and 33-bit recorded float

object Float3ConvertRecFNtoFN{
  def apply(in: Float3): Float3 = {
    if(!in.isRecordedFloat()){
      throw new Exception("input must be in IEEE-754 format before it can be converted to recorded format")
    }
    val out = Wire(new Float3(recorded_float = false))
    out.x := fNFromRecFN(8, 24, in.x)
    out.y := fNFromRecFN(8, 24, in.y)
    out.z := fNFromRecFN(8, 24, in.z)
    out
  }
}

object RayConvertFNtoRecFN{
  def apply(in: Ray): Ray = {
    if(in.isRecordedFloat()){
      throw new Exception("Ray must be in IEEE-754 format before it can be converted to recorded format")
    }

    val out = Wire(new Ray(recorded_float = true))
    out.origin.x := recFNFromFN(8, 24, in.origin.x)
    out.origin.y := recFNFromFN(8, 24, in.origin.y)
    out.origin.z := recFNFromFN(8, 24, in.origin.z)
    out.dir.x := recFNFromFN(8, 24, in.dir.x)
    out.dir.y := recFNFromFN(8, 24, in.dir.y)
    out.dir.z := recFNFromFN(8, 24, in.dir.z)
    out.inv.x := recFNFromFN(8, 24, in.inv.x)
    out.inv.y := recFNFromFN(8, 24, in.inv.y)
    out.inv.z := recFNFromFN(8, 24, in.inv.z)
    out.extent := recFNFromFN(8, 24, in.extent)

    out
  }
}

object RayConvertRecFNtoFN{
  def apply(in: Ray): Ray = {
    if(in.isRecordedFloat() == false){
      throw new Exception("Ray must be in recorded format before it can be converted to IEEE-754 format")
    }

    val out = Wire(new Ray(recorded_float = false))
    out.origin.x := fNFromRecFN(8, 24, in.origin.x)
    out.origin.y := fNFromRecFN(8, 24, in.origin.y)
    out.origin.z := fNFromRecFN(8, 24, in.origin.z)
    out.dir.x := fNFromRecFN(8, 24, in.dir.x)
    out.dir.y := fNFromRecFN(8, 24, in.dir.y)
    out.dir.z := fNFromRecFN(8, 24, in.dir.z)
    out.inv.x := fNFromRecFN(8, 24, in.inv.x)
    out.inv.y := fNFromRecFN(8, 24, in.inv.y)
    out.inv.z := fNFromRecFN(8, 24, in.inv.z)
    out.extent := fNFromRecFN(8, 24, in.extent)

    out
  }
}

object AABBConvertFNtoRecFN{
  def apply(in: AABB): AABB = {
    if(in.isRecordedFloat()){
      throw new Exception("AABB must be in IEEE-754 format before it can be converted to recorded format")
    }

    val out = Wire(new AABB(recorded_float = true))
    out.x_min := recFNFromFN(8, 24, in.x_min)
    out.y_min := recFNFromFN(8, 24, in.y_min)
    out.z_min := recFNFromFN(8, 24, in.z_min)

    out.x_max := recFNFromFN(8, 24, in.x_max)
    out.y_max := recFNFromFN(8, 24, in.y_max)
    out.z_max := recFNFromFN(8, 24, in.z_max)

    out
  }
}

object AABBConvertRecFNtoFN{
  def apply(in: AABB): AABB = {
    if(in.isRecordedFloat() == false){
      throw new Exception("AABB must be in recorded format before it can be converted to IEEE-754 format")
    }

    val out = Wire(new AABB(recorded_float = false))
    out.x_min := fNFromRecFN(8, 24, in.x_min)
    out.y_min := fNFromRecFN(8, 24, in.y_min)
    out.z_min := fNFromRecFN(8, 24, in.z_min)

    out.x_max := fNFromRecFN(8, 24, in.x_max)
    out.y_max := fNFromRecFN(8, 24, in.y_max)
    out.z_max := fNFromRecFN(8, 24, in.z_max)

    out
  }
}