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
  def apply(x: Float): Int = {
    val bb: ByteBuffer = ByteBuffer.allocate(4)
    bb.putFloat(x)
    val y = bb.rewind().getInt()
    y
  }
}

object bitsToFloat{
  def apply(x: Int): Float = {
    val bb: ByteBuffer = ByteBuffer.allocate(4)
    bb.putInt(x)
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
          val hw_sum = bitsToFloat( dut.sum.peek().litValue.intValue )
          println(s"sw: ${sum}, hw: ${hw_sum}, diff_ratio: ${(abs(sum-hw_sum)/sum)}")
        }
        val hw_sum = bitsToFloat( dut.sum.peek().litValue.intValue )
        assert((abs(sum-hw_sum)/sum)<0.01)
      }
    }
  }
}