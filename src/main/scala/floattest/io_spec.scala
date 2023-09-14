package floattest

import chisel3._

class Dual extends Bundle {
  val in1 = SInt(32.W)
  val in2 = SInt(32.W)
}
