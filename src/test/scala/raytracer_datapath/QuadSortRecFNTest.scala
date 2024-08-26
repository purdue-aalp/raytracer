// This file is part of raytracer_chisel.
// Licensed under the BSD 3-clause License.
// See the LICENSE.txt file for details.

package raytracer_datapath

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random
import scala.collection.mutable
import hardfloat._
import raytracer_datapath.raytracer_gold._

// to support Vec literals
import chisel3.experimental.VecLiterals._

object pokeVector {

  /** A convenience method to poke a Vec[T] input port with a sequence of T
    * objects.
    *
    * It makes use of the experimental VecLiteral feature.
    * @param port
    * @param seq
    */
  def apply[T <: Data](port: Vec[T], seq: Seq[T]) = {
    port.poke(
      chiselTypeOf(port).Lit(
        // the ":_*" operator spreads the elements of a sequence into separate arguments
        seq.zipWithIndex.map { case (x, i) => i -> x }: _*
      )
    )
  }
}

class ValExec_QuadSortRecFN extends Module {
  val in = IO(Input(Vec(4, Bits(32.W))))
  val actual_out = IO(Output(Vec(4, Bits(32.W))))
  val expected_out = IO(Input(Vec(4, Bits(32.W))))
  val pass = IO(Output(Bool()))

  val in_33 = Wire(Vec(4, Bits(33.W)))
  val out_33 = Wire(Vec(4, Bits(33.W)))

  // convert input 32->33
  (0 until 4).foreach { n =>
    in_33(n) := recFNFromFN(8, 24, in(n))
  }

  // sort
  val fu = Module(new QuadSortRecFN())
  fu.io.in := in_33
  out_33 := fu.io.out

  // convert output 33->32
  (0 until 4).foreach { n =>
    actual_out(n) := fNFromRecFN(8, 24, out_33(n))
  }

  pass := true.B
  actual_out.zip(expected_out).foreach { case (a, b) =>
    when(a =/= b) { pass := false.B }
  }
}

class ValExec_QuadSortRecFNWithIndex extends Module {
  val in = IO(Input(Vec(4, Bits(32.W))))
  val actual_out = IO(Output(Vec(4, Bits(2.W))))
  val expected_out = IO(Input(Vec(4, Bits(2.W))))
  val pass = IO(Output(Bool()))

  val in_33 = Wire(Vec(4, Bits(33.W)))

  // convert input 32->33
  (0 until 4).foreach { n =>
    in_33(n) := recFNFromFN(8, 24, in(n))
  }

  // sort
  val fu = Module(new QuadSortRecFNWithIndex())
  fu.io.in := in_33
  actual_out := fu.io.sorted_indices

  pass := true.B
  actual_out.zip(expected_out).foreach { case (a, b) =>
    when(a =/= b) { pass := false.B }
  }
}

class QuadSortRecFNTest extends AnyFreeSpec with ChiselScalatestTester {
  val r = new Random()
  val N_TEST = 100000
  "value sorter correctly sort four floats" in {
    test(new ValExec_QuadSortRecFN()).withAnnotations(
      Seq(VerilatorBackendAnnotation)
    ) { dut =>
      for (_ <- 0 until N_TEST) {
        val input_vector = Seq.tabulate(4)(_ => 1e8f * (r.nextFloat() - 0.5f))
        val expected_out = input_vector.sortWith(_ > _)

        pokeVector(dut.in, input_vector.map(floatToBits(_)))
        pokeVector(dut.expected_out, expected_out.map(floatToBits(_)))
        val actual_out = dut.actual_out.peek().map { x => bitsToFloat(x) }

        // println(s"expectation is ${expected_out}, actual is ${actual_out}")
        dut.pass.expect(true)
      }
    }
  }

  "index+value sorter correctly sort four floats" in {
    test(new ValExec_QuadSortRecFNWithIndex()).withAnnotations(
      Seq(VerilatorBackendAnnotation)
    ) { dut =>
      for (_ <- 0 until N_TEST) {
        val input_vector =
          mutable.ArraySeq.tabulate(4)(_ => 1e8f * (r.nextFloat() - 0.5f))

        val ran = r.nextFloat()
        if (ran < 0.1) {
          val swap_idx_with_inf = r.nextInt(4)
          input_vector(swap_idx_with_inf) =
            if (r.nextBoolean()) Float.PositiveInfinity
            else Float.NegativeInfinity
        } else if (ran < 0.2) {
          // this can break
          // input_vector(r.nextInt(4)) = Float.NaN
        }

        val expected_out = input_vector.zipWithIndex
          .sortWith { case ((left, left_idx), (right, right_idx)) =>
            left > right
          }
          .map(_._2)
          .toSeq

        pokeVector(dut.in, input_vector.toSeq.map(floatToBits(_)))
        pokeVector(dut.expected_out, expected_out.map(_.asUInt(2.W)))
        val actual_out = dut.actual_out.peek().map { x => x.litValue }

        // println(s"input is ${input_vector}, expectation is ${expected_out}, actual is ${actual_out}")
        dut.pass.expect(true)
      }
    }
  }
}
