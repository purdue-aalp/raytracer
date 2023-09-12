package baseline_datapath

import chisel3._ 
import hardfloat._

class RecFNCompareSelect extends Module {
  /**
    * Given two input Recorded-format Floats `a` and `b`, output either `c` or
    * `d` depending on the result of comparison:
    * output = (a > b | (a==b && option)) ? c : d  
    */
  val io = IO(new Bundle{
    val a = Input(Bits(33.W))
    val b = Input(Bits(33.W))
    val c = Input(Bits(33.W))
    val d = Input(Bits(33.W))
    val option = Input(Bool())
    val out = Output(Bits(33.W))
  })

  val fu = Module(new CompareRecFN(8, 24))
  fu.io.a := this.io.a 
  fu.io.b := this.io.b 
  fu.io.signaling := true.B 

  when(io.option && fu.io.eq){
    this.io.out := this.io.c
  } .elsewhen(fu.io.gt){
    this.io.out := this.io.c
  } .otherwise {
    this.io.out := this.io.d
  }

}