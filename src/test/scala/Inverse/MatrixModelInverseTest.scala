package gps
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import spire._
import spire.math._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}


object MatrixInverseData {
    type Matrix = Seq[Seq[FixedPoint]]
    val p = GPSParams()
    implicit val scale = p.scale
    val ident2x2  = Seq(Seq(FixedPoint(1.0),FixedPoint(0.0)),
                     Seq(FixedPoint(0.0),FixedPoint(1.0)))

    val in2x2  = Seq(Seq(FixedPoint(1.5),FixedPoint(2.5)),
                     Seq(FixedPoint(3.5),FixedPoint(4.5)))
    val out2x4 = Seq(Seq(FixedPoint(1.5),FixedPoint(2.5),FixedPoint(1.0),FixedPoint(0.0)),
                     Seq(FixedPoint(3.5),FixedPoint(4.5),FixedPoint(0.0),FixedPoint(1.0)))
    val outgauss2x4 = Seq(Seq(FixedPoint(1.0),FixedPoint(0.0),   FixedPoint(-2.25),FixedPoint(1.25)),
                          Seq(FixedPoint(0.0),FixedPoint(1.0),   FixedPoint(1.75),FixedPoint(-0.75)))

}

class MatInverseModelTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MatrixInverseModel"

  it should "Generate Ident Matrix 2x2" in {
    assert(new MatrixInverseModel(MatrixInverseData.p).genIdentity(2) == MatrixInverseData.ident2x2)
  }
  it should "Generate append Matrix 2x2" in {
    assert(new MatrixInverseModel(MatrixInverseData.p).append(MatrixInverseData.in2x2, new MatrixInverseModel(MatrixInverseData.p).genIdentity(2))== MatrixInverseData.out2x4)
  }
  it should("gauss") in {
      assert((new MatrixInverseModel(MatrixInverseData.p).gaussElimination(MatrixInverseData.in2x2, new MatrixInverseModel(MatrixInverseData.p).genIdentity(2))== MatrixInverseData.outgauss2x4))
  }

}