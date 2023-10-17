package baseline_datapath

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random
import hardfloat._
import baseline_datapath.raytracer_gold._

class ValExec_FNFlipSign extends Module {
  val in = IO(Input(Bits(32.W)))
  val actual_out = IO(Output(Bits(32.W)))
  val expected_out = IO(Input(Bits(32.W)))
  val pass = IO(Output(Bool()))

  actual_out := FNFlipSign(in)
  pass := actual_out === expected_out
}

class FNFlipSign_test extends AnyFreeSpec with ChiselScalatestTester {
  val N_TEST = 10000
  val r = new Random()
  "FNFlipSign correctly changes sign of float32 vlaues" in {
    test(new ValExec_FNFlipSign()) { dut =>
      for (_ <- Range(0, N_TEST)) {
        val r_float = r.nextFloat()

        dut.in.poke(floatToBits(r_float))
        dut.expected_out.poke(floatToBits(0.0f - r_float))
        dut.pass.expect(true.B)
      }
    }
  }

  "FNFlipSign handles pos/neg zero correctly" in {
    test(new ValExec_FNFlipSign) { dut =>
      dut.in.poke(0x00000000L.U)
      dut.expected_out.poke(0x80000000L.U)
      dut.pass.expect(true.B)

      dut.in.poke(0x80000000L.U)
      dut.expected_out.poke(0x00000000L.U)
      dut.pass.expect(true.B)

      dut.in.poke(0x00000000L.U)
      dut.expected_out.poke(0x00000000L.U)
      dut.pass.expect(false.B)

      dut.in.poke(0x80000000L.U)
      dut.expected_out.poke(0x80000000L.U)
      dut.pass.expect(false.B)
    }
  }
}
