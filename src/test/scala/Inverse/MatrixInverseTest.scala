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

    val ingauss2x2  = Seq(Seq(FixedPoint(1.5),FixedPoint(2.5)),
                          Seq(FixedPoint(3.5),FixedPoint(4.5)))

    val outgauss2x2 = Seq(Seq(FixedPoint(-2.25),FixedPoint(1.25)),
                          Seq(FixedPoint(1.75),FixedPoint(-0.75)))

    val incholesky2x2  = Seq(Seq(FixedPoint(1.5),FixedPoint(0.5)),
                             Seq(FixedPoint(0.5),FixedPoint(4.5)))                   

    val outcholesky2x2  = Seq(Seq(FixedPoint(1.225),FixedPoint(0.0)),
                             Seq(FixedPoint(0.408),FixedPoint(2.082)))      
}

class MatInverseModelTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MatrixInverseModel"

  it should "Generate Ident Matrix 2x2" in {
    assert(new MatrixInverseModel(MatrixInverseData.p).genIdentity(2) == MatrixInverseData.ident2x2)
  }
  it should "Generate append Matrix 2x2" in {
    assert(new MatrixInverseModel(MatrixInverseData.p).append(MatrixInverseData.in2x2, new MatrixInverseModel(MatrixInverseData.p).genIdentity(2))== MatrixInverseData.out2x4)
  }
  it should("gauss invert") in {
      implicit val scale = MatrixInverseData.p.scale
      val error = (new MatrixInverseModel(MatrixInverseData.p).gaussElimination(MatrixInverseData.in2x2, new MatrixInverseModel(MatrixInverseData.p).genIdentity(2)) zip MatrixInverseData.outgauss2x2).flatMap
      {
        case (a, b) => (a zip b).map
        { 
          case (a,b) => abs((b.toDouble - a.toDouble) / b.toDouble)
        }
      }
      var total_error = 0.0 
      error.foreach(total_error += _)
      println(total_error)
      assert(total_error < 0.05)
      //assert((new MatrixInverseModel(MatrixInverseData.p).gaussElimination(MatrixInverseData.in2x2, new MatrixInverseModel(MatrixInverseData.p).genIdentity(2))== MatrixInverseData.outgauss2x2))
  }
  it should("gauss invert back") in {
      implicit val scale = MatrixInverseData.p.scale
      val error = (new MatrixInverseModel(MatrixInverseData.p).gaussElimination(MatrixInverseData.outgauss2x2, new MatrixInverseModel(MatrixInverseData.p).genIdentity(2)) zip MatrixInverseData.in2x2).flatMap
      {
        case (a, b) => (a zip b).map
        { 
          case (a,b) => abs((b.toDouble - a.toDouble) / b.toDouble)
        }
      }
      var total_error = 0.0 
      error.foreach(total_error += _)
      println(total_error)
      assert(total_error < 0.1)
      //assert((new MatrixInverseModel(MatrixInverseData.p).gaussElimination(MatrixInverseData.outgauss2x2, new MatrixInverseModel(MatrixInverseData.p).genIdentity(2))== MatrixInverseData.in2x2))
  }
  // it should("cholesky decomp") in {
  //     assert((new MatrixInverseModel(MatrixInverseData.p).cholesky(MatrixInverseData.incholesky2x2)== MatrixInverseData.outcholesky2x2))
  // }
  // it should("choleskyWikipedia decomp") in {
  //     assert((new MatrixInverseModel(MatrixInverseData.p).cholesky(MatrixInverseData.incholesky2x2)== MatrixInverseData.outcholesky2x2))
  // }
}