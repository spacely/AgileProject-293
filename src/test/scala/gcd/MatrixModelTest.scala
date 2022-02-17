package gps
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import gps.MatrixTransposeModel.Matrix

object MatrxiTransposeData {
    val in2x2  = Seq(Seq(1,2),
                     Seq(3,4))
    val out2x2 = Seq(Seq(1,3),
                     Seq(2,4))

    val in2x3  = Seq(Seq(1,2,3),
                     Seq(3,5, 6))
    val out3x2 = Seq(Seq(1,3),
                     Seq(2,5),
                     Seq(3,6))

    val in1x6  = Seq(Seq(1,2,3,4,5,6))
    val out6x1 = Seq(Seq(1),
                     Seq(2),
                     Seq(3),
                     Seq(4),
                     Seq(5),
                     Seq(6))
}

class MatMulModelTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MatrixTransposeModel"

  it should "Tranpose Matrix of size 2x2" in {
    assert(MatrixTransposeModel(MatrxiTransposeData.in2x2) == MatrxiTransposeData.out2x2)
  }
  it should "Tranpose Matrix of size 2x3" in {
    assert(MatrixTransposeModel(MatrxiTransposeData.in2x3) == MatrxiTransposeData.out3x2)
  }
    it should "Tranpose Matrix of size 1x6" in {
    assert(MatrixTransposeModel(MatrxiTransposeData.in1x6) == MatrxiTransposeData.out6x1)
  }
}