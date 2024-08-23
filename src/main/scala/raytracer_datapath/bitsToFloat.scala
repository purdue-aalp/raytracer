package raytracer_datapath

import chisel3._
import java.nio.ByteBuffer

object bitsToFloat {
  def apply(x: UInt): Float = {
    assert(x.getWidth == 32)
    val bb: ByteBuffer = ByteBuffer.allocate(4)
    bb.putInt(x.litValue.intValue)
    val y = bb.rewind().getFloat()
    y
  }
}
