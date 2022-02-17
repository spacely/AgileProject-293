package gps
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import gps.MatrixInverseModel.Matrix

object MatrixInverseData {
    val ident2x2  = Seq(Seq(1,0),
                     Seq(0,1))

    val in2x2  = Seq(Seq(1,2),
                     Seq(3,4))
    val out2x4 = Seq(Seq(1,2,1,0),
                     Seq(3,4,0,1))
    val outgauss2x4 = Seq(Seq(1,0,   -2,1),
                          Seq(0,1,   1,0))

}

class MatInverseModelTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MatrixInverseModel"

  it should "Generate Ident Matrix 2x2" in {
    assert(MatrixInverseModel.genIdentity(2) == MatrixInverseData.ident2x2)
  }
  it should "Generate append Matrix 2x2" in {
    assert(MatrixInverseModel.append(MatrixInverseData.in2x2, MatrixInverseModel.genIdentity(2))== MatrixInverseData.out2x4)
  }
//   it should "gauss 2x2" in {
//     assert(MatrixInverseModel.gauss(MatrixInverseModel.append(MatrixInverseData.in2x2, MatrixInverseModel.genIdentity(2)))== MatrixInverseData.out2x4)
//   }
  it should("gauss") in {
      assert((MatrixInverseModel.gaussElimination(MatrixInverseData.in2x2, MatrixInverseModel.genIdentity(2))== MatrixInverseData.outgauss2x4))
  }

}