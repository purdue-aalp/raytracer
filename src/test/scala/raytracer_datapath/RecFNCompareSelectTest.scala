package raytracer_datapath

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random
import hardfloat._
import raytracer_datapath.raytracer_gold._

import chisel3.stage.{
  PrintFullStackTraceAnnotation,
  ThrowOnFirstErrorAnnotation
}

class ValExec_RecFNCompareSelect(
    val option: Boolean
) extends Module {
  val io = IO(new Bundle {
    val a = Input(Bits(32.W))
    val b = Input(Bits(32.W))
    val c = Input(Bits(32.W))
    val d = Input(Bits(32.W))

    val actual = new Bundle {
      val c_out = Output(Bits(32.W))
      val d_out = Output(Bits(32.W))
    }

    val expected = new Bundle {
      val c_out = Input(Bits(32.W))
      val d_out = Input(Bits(32.W))
    }

    val pass = Output(Bool())
  })

  val unit = Module(
    new RecFNCompareSelect(option = option, passthrough_type = Bits(33.W))
  )
  unit.io.a := recFNFromFN(8, 24, io.a)
  unit.io.b := recFNFromFN(8, 24, io.b)
  unit.io.c := recFNFromFN(8, 24, io.c)
  unit.io.d := recFNFromFN(8, 24, io.d)

  io.actual.c_out := fNFromRecFN(8, 24, unit.io.c_out)
  io.actual.d_out := fNFromRecFN(8, 24, unit.io.d_out)

  when(
    io.expected.c_out === io.actual.c_out && io.expected.d_out === io.actual.d_out
  ) {
    io.pass := true.B
  }.otherwise {
    io.pass := false.B
  }
}

class RecFNCompareSelectTest extends AnyFreeSpec with ChiselScalatestTester {
  val r = new Random()
  val N_TEST = 100000
  "select the greater when option is false" in {
    test(new ValExec_RecFNCompareSelect(false)).withAnnotations(
      Seq(VerilatorBackendAnnotation)
    ) { dut =>
      for (_ <- 0 to N_TEST) {
        val a = 1e8f * (r.nextFloat() - 0.5f)
        val b = 1e8f * (r.nextFloat() - 0.5f)
        val c = 1e8f * (r.nextFloat() - 0.5f)
        val d = 1e8f * (r.nextFloat() - 0.5f)
        dut.io.a.poke(floatToBits(a))
        dut.io.b.poke(floatToBits(b))
        dut.io.c.poke(floatToBits(c))
        dut.io.d.poke(floatToBits(d))

        var expected_c_out: Float = 0.0f
        var expected_d_out: Float = 0.0f
        if (a > b) {
          expected_c_out = c
          expected_d_out = d
        } else {
          expected_c_out = d
          expected_d_out = c
        }
        dut.io.expected.c_out.poke(floatToBits(expected_c_out))
        dut.io.expected.d_out.poke(floatToBits(expected_d_out))

        val pass = dut.io.pass.peek()

        // println(s"a,b,c,d,expected,actual = $a, $b, $c, $d, ($expected_c_out, $expected_d_out), (${bitsToFloat(dut.io.actual.c_out.peek())}, ${bitsToFloat(dut.io.actual.d_out.peek())})")
        assert(pass.litToBoolean)
      }
    }
  }

  "select d when option is false and a==b" in {
    test(new ValExec_RecFNCompareSelect(false))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        for (_ <- 0 to N_TEST) {
          val a = 1e8f * (r.nextFloat() - 0.5f)
          val b = a
          val c = 1e8f * (r.nextFloat() - 0.5f)
          val d = 1e8f * (r.nextFloat() - 0.5f)
          dut.io.a.poke(floatToBits(a))
          dut.io.b.poke(floatToBits(b))
          dut.io.c.poke(floatToBits(c))
          dut.io.d.poke(floatToBits(d))

          var expected_c_out: Float = 0.0f
          var expected_d_out: Float = 0.0f
          if (a > b) {
            expected_c_out = c
            expected_d_out = d
          } else {
            expected_c_out = d
            expected_d_out = c
          }
          dut.io.expected.c_out.poke(floatToBits(expected_c_out))
          dut.io.expected.d_out.poke(floatToBits(expected_d_out))

          val pass = dut.io.pass.peek()

          // println(s"a,b,c,d,expected,actual = $a, $b, $c, $d, ($expected_c_out, $expected_d_out), (${bitsToFloat(dut.io.actual.c_out.peek())}, ${bitsToFloat(dut.io.actual.d_out.peek())})")
          assert(pass.litToBoolean)
        }
      }
  }

  "select c when option is true and a==b" in {
    test(new ValExec_RecFNCompareSelect(true))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        for (_ <- 0 to N_TEST) {
          val a = 1e8f * (r.nextFloat() - 0.5f)
          val b = a
          val c = 1e8f * (r.nextFloat() - 0.5f)
          val d = 1e8f * (r.nextFloat() - 0.5f)
          dut.io.a.poke(floatToBits(a))
          dut.io.b.poke(floatToBits(b))
          dut.io.c.poke(floatToBits(c))
          dut.io.d.poke(floatToBits(d))

          var expected_c_out: Float = 0.0f
          var expected_d_out: Float = 0.0f
          if (a >= b) {
            expected_c_out = c
            expected_d_out = d
          } else {
            expected_c_out = d
            expected_d_out = c
          }
          dut.io.expected.c_out.poke(floatToBits(expected_c_out))
          dut.io.expected.d_out.poke(floatToBits(expected_d_out))

          val pass = dut.io.pass.peek()

          // println(s"a,b,c,d,expected,actual = $a, $b, $c, $d, ($expected_c_out, $expected_d_out), (${bitsToFloat(dut.io.actual.c_out.peek())}, ${bitsToFloat(dut.io.actual.d_out.peek())})")
          assert(pass.litToBoolean)
        }
      }
  }
}
