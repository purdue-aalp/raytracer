package baseline_datapath

import chisel3._
import scala.language.implicitConversions

class F32 {
  val wrapped = UInt(32.W)
}

object F32 {
  implicit def F32toUInt32(x : F32): UInt = {
    x.wrapped
  }
  implicit def UInt32toF32(x : UInt): F32 = {
    val y = new F32()
    y.wrapped := x 
    y 
  }
}
