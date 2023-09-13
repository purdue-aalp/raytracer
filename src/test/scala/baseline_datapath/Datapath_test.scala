package baseline_datapath

import chisel3._ 
import chiseltest._ 
import org.scalatest.freespec.AnyFreeSpec 
import scala.util.Random 
import scala.math._
import baseline_datapath.raytracer_gold._

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

"Small Ray inside box" in {
  val b = Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
  val r = new Ray(float_3(0.5f, -0.5f, 0.5f), float_3(0.0001f, -0.0001f, -0.001f))
  val result = RaytracerGold.testIntersection(r, b)
  print(result)
}

"Ray outside box pointing away" in {
  val b = Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
  val r = new Ray(float_3(2.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f))
  val result = RaytracerGold.testIntersection(r, b)
  print(result)
}

"Ray on edge of box pointing away" in {
  val b = Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
  val r = new Ray(float_3(1.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f))
  val result = RaytracerGold.testIntersection(r, b)
  print(result)
}

"Ray on corner of box pointing away" in {
  val b = Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
  val r = new Ray(float_3(1.0f, 1.0f, 1.0f), float_3(1.0f, 1.0f, 1.0f))
  val result = RaytracerGold.testIntersection(r, b)
  print(result)
}

"Ray on corner of box pointing along edge" in {
  val b = Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
  val r = new Ray(float_3(1.0f, 1.0f, 1.0f), float_3(0.0f, -1.0f, 0.0f))
  val result = RaytracerGold.testIntersection(r, b)
  print(result)
}

"Ray outside box pointing towards box" in {
  val b = Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
  val r = new Ray(float_3(-2.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f))
  val result = RaytracerGold.testIntersection(r, b)
  print(result)
}

  "Ray hits node 1 then node 2" in {}
  "Ray hits node 4 then 1 then 2 then misses 3" in {}
  "Ray outside box pointing along edge" in {}

}