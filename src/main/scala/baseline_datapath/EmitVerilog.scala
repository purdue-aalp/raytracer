package baseline_datapath

import chisel3._
import chisel3.util.Decoupled
import circt.stage.ChiselStage
import hardfloat._

class Foo(
    inExp: Int = 8,
    inSig: Int = 26,
    outExp: Int,
    outSig: Int
) extends Module {
  val in = IO(Input(new RawFloat(inExp, inSig)))
  val out = IO(Output(Bits((outExp + outSig + 1).W)))

  val roundAndConvert = Module(
    new RoundAnyRawFNToRecFN(
      inExpWidth = inExp,
      inSigWidth = inSig,
      outExpWidth = outExp,
      outSigWidth = outSig,
      options = 0
    )
  )

  roundAndConvert.io.invalidExc := false.B
  roundAndConvert.io.infiniteExc := false.B
  roundAndConvert.io.in := in
  roundAndConvert.io.roundingMode := consts.round_near_even
  roundAndConvert.io.detectTininess := consts.tininess_afterRounding

  out := roundAndConvert.io.out
}

class Bar(
    inExp: Int = 8,
    inSig: Int = 24,
    outExp: Int,
    outSig: Int
) extends Module {
  val in = IO(Input(Bits((inExp + inSig + 1).W)))
  val out = IO(Output(Bits((outExp + outSig + 1).W)))

  val convert = Module(
    new RecFNToRecFN(
      inExpWidth = inExp,
      inSigWidth = inSig,
      outExpWidth = outExp,
      outSigWidth = outSig
    )
  )

  convert.io.in := in
  convert.io.roundingMode := consts.round_near_even
  convert.io.detectTininess := consts.tininess_beforeRounding

  out := convert.io.out
}

class ChainedSkidBufferStages extends Module {
  val intake = IO(Flipped(Decoupled(UInt(10.W))))
  val emit = IO(Decoupled(UInt(10.W)))

  val stage1 = Module(SkidBufferStage(UInt(10.W), { (x: UInt) => x + 9.U }))
    .suggestName("stage1")
  val stage2 =
    Module(SkidBufferStage(UInt(10.W), { (x: UInt) => x * 11.U }))
      .suggestName("stage2")
  val stage3 = Module(SkidBufferStage(UInt(10.W), { (x: UInt) => x - 9.U }))
    .suggestName("stage3")

  stage1.intake :<>= intake
  stage2.intake :<>= stage1.emit
  stage3.intake :<>= stage2.emit
  emit :<>= stage3.emit
}

object EmitVerilog extends App {
  var shortest_length = Double.PositiveInfinity
  var shortest_code = new String()

  // for(expW <- 7 to 10; sigW <- 24 to 28){
  //   val sv_code = ChiselStage.emitSystemVerilog(new Foo(outExp = expW, outSig = sigW))
  //   val length = sv_code.length()

  //   println(s"expW=${expW}, sigW=${sigW}, length=${length}")

  //   if(length < shortest_length){
  //     println("shortest found!")
  //     shortest_length = length.toDouble
  //     shortest_code = sv_code
  //   }
  // }

  // print(shortest_code)

  // val sv_code = ChiselStage.emitSystemVerilog(new Bar(outExp = 9, outSig =
  // 27))
  val sv_code = ChiselStage.emitSystemVerilog(new ChainedSkidBufferStages)
  print(sv_code)
}
