package floattest

import chisel3._ 
import chiseltest._ 
import org.scalatest.freespec.AnyFreeSpec

class FloatTestSpec extends AnyFreeSpec with ChiselScalatestTester {
  "blah" in {
    test(new FloatCalc) {dut => 
      dut.io.in1.poke(17)
      dut.io.in2.poke(9)

      dut.clock.step(10)
      val out = dut.io.out.peek()
      print(s"print something like ${out}")
    }
  }
}