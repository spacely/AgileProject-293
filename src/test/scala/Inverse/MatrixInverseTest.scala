package gps
import chisel3._
//import chiseltest.{ChiselScalatestTester, WriteVcdAnnotation, testableClock, testableFixedPoint, testableSInt, DecoupledDriver}
import chiseltest.{iotesters, defaults, ChiselScalatestTester, WriteVcdAnnotation, testableFixedPoint, ValidDriver, DecoupledDriver, testableBool, testableClock}
//import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import spire._
import spire.math._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}

import gps.MatrixTransposeModel.Matrix

object MatrixInverseData {
    val p = GPSParams()
    implicit val scale = p.scale
    val ident2x2  = Seq(Seq(FixedPoint(1.0),FixedPoint(0.0)),
                     Seq(FixedPoint(0.0),FixedPoint(1.0)))

    val in2x2  = Seq(Seq(FixedPoint(1.5),FixedPoint(2.5)),
                     Seq(FixedPoint(3.5),FixedPoint(4.5)))

    val in4x4  = Seq(Seq(FixedPoint(1.5),FixedPoint(2.5),FixedPoint(3.5),FixedPoint(4.5)),
                     Seq(FixedPoint(1.5),FixedPoint(2.5),FixedPoint(3.5),FixedPoint(4.5)),
                     Seq(FixedPoint(1.5),FixedPoint(2.5),FixedPoint(3.5),FixedPoint(4.5)),
                     Seq(FixedPoint(3.5),FixedPoint(4.5),FixedPoint(3.5),FixedPoint(4.5)))

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

class MatrixInverseModelTester extends AnyFlatSpec with ChiselScalatestTester {
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
      println("percent error: " + total_error)
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
      println("percent error: " + total_error)
      //assert((new MatrixInverseModel(MatrixInverseData.p).gaussElimination(MatrixInverseData.outgauss2x2, new MatrixInverseModel(MatrixInverseData.p).genIdentity(2))== MatrixInverseData.in2x2))
  }
  // it should("cholesky decomp") in {
  //     assert((new MatrixInverseModel(MatrixInverseData.p).cholesky(MatrixInverseData.incholesky2x2)== MatrixInverseData.outcholesky2x2))
  // }
  // it should("choleskyWikipedia decomp") in {
  //     assert((new MatrixInverseModel(MatrixInverseData.p).cholesky(MatrixInverseData.incholesky2x2)== MatrixInverseData.outcholesky2x2))
  // }
}

class MatrixInverseTester extends AnyFlatSpec with ChiselScalatestTester {
    def doMatrixInverseTest(a: Matrix): Boolean = {
    val p = GPSParams(mat_override = true, width = 16, bp = 8, rows_override = a.size, cols_override = a(0).size)
    implicit val scale = p.scale
    val model = MatrixTransposeModel(a)

    test(new MatrixInverse(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.in.ready.expect(true.B)
      for (r <- 0 until p.rows) {
        for (c <- 0 until p.cols) {
          dut.io.in.bits.a(r)(c).poke(p.fixedToChisel(a(r)(c)))
        }
      }
      dut.clock.step(100)
      // for (r <- 0 until model.size) {
      //   for (c <- 0 until model(0).size) {
      //     dut.io.out.bits(r)(c).expect(p.fixedToChisel(model(r)(c)))
      //   }
      // }
      
      }
    true
    }

  behavior of "MatrixInverse"
  it should "Divide Fixed Points 1/2 = 0.5" in {
    val p = GPSParams(width = 8, bp = 4)

    test(new MatrixDivide(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.a.poke(experimental.FixedPoint.fromDouble(1.0, p.width.W, p.bp.BP))
      dut.io.b.poke(experimental.FixedPoint.fromDouble(2.0, p.width.W, p.bp.BP))
      dut.io.z.expect(experimental.FixedPoint.fromDouble(0.5, p.width.W, p.bp.BP))
    }
    true
  }
  it should "Divide Fixed Points 1/4 = 0.25" in {
    val p = GPSParams(width = 8, bp = 4)

    test(new MatrixDivide(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.a.poke(experimental.FixedPoint.fromDouble(1.0, p.width.W, p.bp.BP))
      dut.io.b.poke(experimental.FixedPoint.fromDouble(4.0, p.width.W, p.bp.BP))
      dut.io.z.expect(experimental.FixedPoint.fromDouble(0.25, p.width.W, p.bp.BP))
    }
    true
  }
  it should "Divide Fixed Points 2/4 = 0.5" in {
    val p = GPSParams(width = 8, bp = 4)

    test(new MatrixDivide(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.a.poke(experimental.FixedPoint.fromDouble(2.0, p.width.W, p.bp.BP))
      dut.io.b.poke(experimental.FixedPoint.fromDouble(4.0, p.width.W, p.bp.BP))
      dut.io.z.expect(experimental.FixedPoint.fromDouble(0.5, p.width.W, p.bp.BP))
    }
    true
  }
  it should "Inverse Matrix of size 2x2" in {
    doMatrixInverseTest(MatrixInverseData.in2x2)
  }
  it should "Inverse Matrix of size 4x4" in {
    doMatrixInverseTest(MatrixInverseData.in4x4)
  }
}