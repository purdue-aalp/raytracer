package baseline_datapath

import chisel3._ 
import chiseltest._ 
import org.scalatest.freespec.AnyFreeSpec 
import scala.util.Random 
import hardfloat._

class ValExec_RecFNCompareSelect extends Module{
  val io = IO(new Bundle {
    val a = Input(Bits(32.W))
    val b = Input(Bits(32.W))
    val c = Input(Bits(32.W))
    val d = Input(Bits(32.W))
    val option = Input(Bool())

    val actual = new Bundle {
      val out = Output(Bits(32.W))
    }

    val expected = new Bundle {
      val out = Input(Bits(32.W))
    }

    val pass = Output(Bool())
  })

  val unit = Module(new RecFNCompareSelect)
  unit.io.a := recFNFromFN(8, 24, io.a) 
  unit.io.b := recFNFromFN(8, 24, io.b) 
  unit.io.c := recFNFromFN(8, 24, io.c) 
  unit.io.d := recFNFromFN(8, 24, io.d) 
  unit.io.option := io.option

  io.actual.out := fNFromRecFN(8, 24, unit.io.out )

  when(io.expected.out === io.actual.out){
    io.pass := true.B
  }.otherwise{
    io.pass := false.B
  }
}

class RecFNCompareSelectTest extends AnyFreeSpec with ChiselScalatestTester {
  val r = new Random() 
  val N_TEST = 50000
  "select the greater when option is false" in {
    test(new ValExec_RecFNCompareSelect).withAnnotations(
      Seq(VerilatorBackendAnnotation)){dut=>
        for(_ <- 0 to N_TEST){
          val a = r.nextFloat()
          val b = r.nextFloat()
          val c = r.nextFloat()
          val d = r.nextFloat() 
          dut.io.a.poke(floatToBits(a))
          dut.io.b.poke(floatToBits(b))
          dut.io.c.poke(floatToBits(c))
          dut.io.d.poke(floatToBits(d))
          dut.io.option.poke(false.B)

          val expected = {
            if(a>b) c
            else d
          }
          dut.io.expected.out.poke(floatToBits(expected))

          val pass = dut.io.pass.peek()
          // println(s"a,b,c,d,expected,actual = $a, $b, $c, $d, $expected, ${bitsToFloat(dut.io.actual.out.peek().litValue.intValue)}")
          assert(pass.litToBoolean)
        }
    }
  }

  "select d when option is false and a==b" in {
    test(new ValExec_RecFNCompareSelect).withAnnotations(
      Seq(VerilatorBackendAnnotation)){dut=>
        for(_ <- 0 to N_TEST){
          val a = r.nextFloat()
          val b = a
          val c = r.nextFloat()
          val d = r.nextFloat() 
          dut.io.a.poke(floatToBits(a))
          dut.io.b.poke(floatToBits(b))
          dut.io.c.poke(floatToBits(c))
          dut.io.d.poke(floatToBits(d))
          dut.io.option.poke(false.B)

          val expected = {
            if(a>b) c
            else d
          }
          dut.io.expected.out.poke(floatToBits(expected))

          val pass = dut.io.pass.peek()
          // println(s"a,b,c,d,expected,actual = $a, $b, $c, $d, $expected, ${bitsToFloat(dut.io.actual.out.peek().litValue.intValue)}")
          assert(pass.litToBoolean)
        }
    }
  }

  "select c when option is true and a==b" in {
    test(new ValExec_RecFNCompareSelect).withAnnotations(
      Seq(VerilatorBackendAnnotation)){dut=>
        for(_ <- 0 to N_TEST){
          val a = r.nextFloat()
          val b = a
          val c = r.nextFloat()
          val d = r.nextFloat() 
          dut.io.a.poke(floatToBits(a))
          dut.io.b.poke(floatToBits(b))
          dut.io.c.poke(floatToBits(c))
          dut.io.d.poke(floatToBits(d))
          dut.io.option.poke(true.B)

          val expected = {
            if(a>=b) c
            else d
          }
          dut.io.expected.out.poke(floatToBits(expected))

          val pass = dut.io.pass.peek()
          // println(s"a,b,c,d,expected,actual = $a, $b, $c, $d, $expected, ${bitsToFloat(dut.io.actual.out.peek().litValue.intValue)}")
          assert(pass.litToBoolean)
        }
    }
  }
}
