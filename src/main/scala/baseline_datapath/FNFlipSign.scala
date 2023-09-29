package baseline_datapath

import chisel3.Bits

/**
  * Flip the sign bit (MSB) for a floating point or recorded flating point
  * nnumber alike. They both use the MSB to represent sign. 
  */
object FNFlipSign {
  def apply(in: Bits) = {
    assert(in.widthKnown)
    val w = in.getWidth 

    (~in(w-1)) ## in(w-2, 0)
  }
}
