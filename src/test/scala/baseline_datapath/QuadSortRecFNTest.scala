package baseline_datapath

import chisel3._ 
import chiseltest._ 
import org.scalatest.freespec.AnyFreeSpec 
import scala.util.Random 
import hardfloat._

class ValExec_QuadSortRecFN extends Module {
  val in = IO(Input(Vec(4, Bits(32.W))))
  val actual_out = IO(Output(Vec(4, Bits(32.W))))
  val expected_out = IO(Input(Vec(4, Bits(32.W))))
  val pass = IO(Output(Bool()))

  val in_33 = Wire(Vec(4, Bits(33.W)))
  val out_33 = Wire(Vec(4, Bits(33.W)))

  // convert input 32->33
  (0 until 4).foreach{n => 
    in_33(n) := recFNFromFN(8, 24, in(n))
  }

  // sort 
  val fu = Module(new QuadSortRecFN())
  fu.io.in := in_33 
  out_33 := fu.io.out

  //convert output 33->32
  (0 until 4).foreach{n => 
    actual_out(n) := fNFromRecFN(8, 24, out_33(n))
  } 

  pass := true.B 
  actual_out.zip(expected_out).foreach{case(a, b)=>
    when(a=/=b)  {pass := false.B}
  }
}

class QuadSortRecFNTest extends AnyFreeSpec with ChiselScalatestTester {
  val r = new Random() 
  val N_TEST = 1000
  "correctly sort four floats" in {
    test(new ValExec_QuadSortRecFN()){dut => 
      for(_ <- 0 until N_TEST){
        val input_vector = Seq.tabulate(4)(_=>r.nextFloat())
        val expected_out = input_vector.sortWith(_ > _)
        dut.in.poke(VecInit(input_vector.map(floatToBits(_).U)))
        dut.expected_out.poke(VecInit(expected_out.map(floatToBits(_).U)))
        println(s"input_vector is ${input_vector}, expectation is ${expected_out}")
        dut.pass.expect(true)
      }  
    }
  }
}
