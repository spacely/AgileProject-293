package gps

import chisel3._
import chiseltest._
import chisel3.experimental
import org.scalatest.flatspec.AnyFlatSpec

import spire._
import spire.math._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}

import gps.MatrixTransposeModel.Matrix


object MatrixTransposeData {
    val p = GPSParams()
    implicit val scale = p.scale

    val in2x2  = Seq(Seq(FixedPoint(1.5),FixedPoint(2.5)),
                     Seq(FixedPoint(3.5),FixedPoint(4.5)))

    val out2x2 = Seq(Seq(FixedPoint(1.5),FixedPoint(3.5)),
                     Seq(FixedPoint(2.5),FixedPoint(4.5)))

    val in2x3  = Seq(Seq(FixedPoint(1.5),FixedPoint(2.5),FixedPoint(3.5)),
                     Seq(FixedPoint(3.5),FixedPoint(5.5),FixedPoint(6.5)))

    val out3x2 = Seq(Seq(FixedPoint(1.5),FixedPoint(3.5)),
                     Seq(FixedPoint(2.5),FixedPoint(5.5)),
                     Seq(FixedPoint(3.5),FixedPoint(6.5)))

    val in1x6  = Seq(Seq(FixedPoint(1.5),FixedPoint(2.5),FixedPoint(3.5),FixedPoint(4.5),FixedPoint(5.5),FixedPoint(6.5)))
    
    val out6x1 = Seq(Seq(FixedPoint(1.5)),
                     Seq(FixedPoint(2.5)),
                     Seq(FixedPoint(3.5)),
                     Seq(FixedPoint(4.5)),
                     Seq(FixedPoint(5.5)),
                     Seq(FixedPoint(6.5)))
}

class MatrixTransposeModelTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MatrixTransposeModel"

  it should "Transpose Matrix of size 2x2" in {
    assert(MatrixTransposeModel(MatrixTransposeData.in2x2) == MatrixTransposeData.out2x2)
  }
  it should "Transpose Matrix of size 2x3" in {
    assert(MatrixTransposeModel(MatrixTransposeData.in2x3) == MatrixTransposeData.out3x2)
  }
    it should "Transpose Matrix of size 1x6" in {
    assert(MatrixTransposeModel(MatrixTransposeData.in1x6) == MatrixTransposeData.out6x1)
  }
}

class MatrixTransposeTester extends AnyFlatSpec with ChiselScalatestTester {

  def doMatrixTransposeTest(a: Matrix): Boolean = {
    val p = GPSParams(mat_override = true, width = 8, bp = 4, rows_override = a.size, cols_override = a(0).size)
    implicit val scale = p.scale
    val model = MatrixTransposeModel(a)

    test(new MatrixTranspose(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.in.valid.poke(true.B)
      for (r <- 0 until p.rows) {
        for (c <- 0 until p.cols) {
          dut.io.in.bits.a(r)(c).poke(p.fixedToChisel(a(r)(c)))
        }
      }
      
      for (r <- 0 until model.size) {
        for (c <- 0 until model(0).size) {
          dut.io.out.bits(r)(c).expect(p.fixedToChisel(model(r)(c)))
        }
      }
      dut.io.out.valid.expect(true.B)
    }
    true
  }

  behavior of "MatrixTranspose"
  it should "Transpose Matrix of size 2x2" in {
    doMatrixTransposeTest(MatrixTransposeData.in2x2)
  }
  it should "Transpose Matrix of size 2x3" in {
    doMatrixTransposeTest(MatrixTransposeData.in2x3)
  }
  it should "Transpose Matrix of size 1x6" in {
    doMatrixTransposeTest(MatrixTransposeData.in1x6)
  }

}

