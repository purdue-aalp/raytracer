package floattest

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import java.nio.ByteBuffer

class FloatTestSpec extends AnyFreeSpec with ChiselScalatestTester {
  "blah" in {
    test(new FloatCalc) { dut =>
      dut.dual_in.in1.poke(179)
      dut.dual_in.in2.poke(9)

      for (i <- 0 to 100) {
        val x = dut.io.out.peek()
        val bb: ByteBuffer = ByteBuffer.allocate(4)
        bb.putInt(x.litValue.intValue)
        val f = bb.rewind().getFloat()
        println(
          s"ready: ${dut.io.out_valid.peek().litToBoolean}, output: ${x.litValue}, converted: ${f}"
        )

        dut.clock.step()
      }
    }
  }
}
