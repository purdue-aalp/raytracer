package playground

import chisel3._
import chisel3.experimental.dataview._
import circt.stage.ChiselStage

class Triple extends Bundle{
  val a = UInt(8.W)
  val b = UInt(8.W)
  val c = UInt(8.W)

  def x_view = {
    val a_w = Wire(chiselTypeOf(a))
    val b_w = Wire(chiselTypeOf(b))
    a_w := a
    b_w := b
    (a_w ## b_w)(15, 4)
  }

  def Double_view: Double = {
    val a_w = Wire(chiselTypeOf(a))
    val b_w = Wire(chiselTypeOf(b))
    val c_w = Wire(chiselTypeOf(c))
    a_w := a
    b_w := b
    c_w := c 
    val double_bundle_wire = Wire(new Double)
    double_bundle_wire.x := (a_w ## b_w)(15, 4)
    double_bundle_wire.y := (b_w ## c_w)(11, 0)
    double_bundle_wire
  }

  // def y_view = {
  //   val b_w = Wire(b)
  //   val c_w = Wire(c)
  //   (b_w ## c_w)(11,0)
  // }
}

class Double extends Bundle{
  val x = UInt(12.W)
  val y = UInt(12.W)
}

class TwoShortVecs extends Bundle{
  val v0 = Vec(4, UInt(8.W))
  val v1 = Vec(4, UInt(8.W))
}

class OneLongVec extends Bundle{
  val v = Vec(8, UInt(8.W))
  
}

class Dataview_Try_Module extends Module{
  val in = IO(Input(new TwoShortVecs))
  val out = IO(Output(new OneLongVec))

  (out.v.getElements zip (in.v0.getElements :++ in.v1.getElements)).foreach{case(o, i) => 
    o := i  
  }
}

