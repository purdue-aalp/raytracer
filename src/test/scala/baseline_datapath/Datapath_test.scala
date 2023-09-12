package baseline_datapath

import chisel3._ 
import chiseltest._ 
import org.scalatest.freespec.AnyFreeSpec 
import scala.util.Random 
import scala.math._

import java.nio.ByteBuffer 

object floatToBits{
  /**
    * Given a Scala float, returns an Int directly casted from the bits of the
    * float.
    * 
    * Similar to C-code: int x; return y = *((float*)(&x));
    *
    * @param x A Float value
    * @return The integer cast of x.
    */
  def apply(x: Float)(implicit width: Int = 32): UInt = {
    val bb: ByteBuffer = ByteBuffer.allocate(8)
    bb.putInt(0) 
    bb.putFloat(x) 
    
    // bb now looks like [00][00][00][00][x3][x2][x1][x0]
    // The reason we allocate 8 bytes and pad the first four with zeros is to
    // prevent negative x values from triggering a "UInt cannot be negative"
    // error from Chisel.
    // y will get truncated anyway
    val y = bb.rewind().getLong()
    y.U(width.W)
  }
}

object bitsToFloat{
  def apply(x: UInt): Float = {
    val bb: ByteBuffer = ByteBuffer.allocate(4)
    bb.putInt(x.litValue.intValue)
    val y = bb.rewind().getFloat()
    y
  }
}

class Datapath_test extends AnyFreeSpec with ChiselScalatestTester{
  val r = new Random()

  "calculated sum should be close if not equal" in {
    test(new Datapath).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ){dut =>

      def update_random_inputs(dut: Datapath): Float = {
        val input_ports = Seq(dut.ray.dir.x, dut.ray.dir.y, dut.ray.dir.z, dut.ray.origin.x, dut.ray.origin.y, dut.ray.origin.z, dut.ray.inv.x, dut.ray.inv.y, dut.ray.inv.z, dut.ray.extent, dut.aabb.x_min, dut.aabb.x_max, dut.aabb.y_min, dut.aabb.y_max, dut.aabb.z_min, dut.aabb.z_max)
        val corresponding_rand_floats =  input_ports.map(_=>r.nextFloat())
        val corresponding_rand_bits = corresponding_rand_floats.map(floatToBits(_))
        val sum = corresponding_rand_floats.sum 

        // assignment
        input_ports.zip(corresponding_rand_bits).map{case (x,y)=>x.poke(y)}

        sum
      }

      for(_ <- 0 to 20){
        val sum = update_random_inputs(dut)
        for(_ <- 0 until 3){
          dut.clock.step(1)
          val hw_sum = bitsToFloat( dut.isIntersect.peek() )
          println(s"sw: ${sum}, hw: ${hw_sum}, diff_ratio: ${(abs(sum-hw_sum)/sum)}")
        }
        val hw_sum = bitsToFloat( dut.isIntersect.peek())
        assert((abs(sum-hw_sum)/sum)<0.01)
      }
    }
  }
}