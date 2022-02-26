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


object MatrxiTransposeData {
    val p = GPSParams()
    implicit val scale = p.scale

    val in2x2  = Seq(Seq(FixedPoint(1.0),FixedPoint(2.0)),
                     Seq(FixedPoint(3.0),FixedPoint(4.0)))

    val out2x2 = Seq(Seq(FixedPoint(1.0),FixedPoint(3.0)),
                     Seq(FixedPoint(2.0),FixedPoint(4.0)))

    val in2x3  = Seq(Seq(FixedPoint(1.0),FixedPoint(2.0),FixedPoint(3.0)),
                     Seq(FixedPoint(3.0),FixedPoint(5.0),FixedPoint(6.0)))

    val out3x2 = Seq(Seq(FixedPoint(1.0),FixedPoint(3.0)),
                     Seq(FixedPoint(2.0),FixedPoint(5.0)),
                     Seq(FixedPoint(3.0),FixedPoint(6.0)))

    val in1x6  = Seq(Seq(FixedPoint(1.0),FixedPoint(2.0),FixedPoint(3.0),FixedPoint(4.0),FixedPoint(5.0),FixedPoint(6.0)))
    val out6x1 = Seq(Seq(FixedPoint(1.0)),
                     Seq(FixedPoint(2.0)),
                     Seq(FixedPoint(3.0)),
                     Seq(FixedPoint(4.0)),
                     Seq(FixedPoint(5.0)),
                     Seq(FixedPoint(6.0)))
}

class MatrixTranposeModelTester extends AnyFlatSpec with ChiselScalatestTester {
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

class MatrixTranposeTester extends AnyFlatSpec with ChiselScalatestTester {

  def doMatrixTransposeTest(a: Matrix, sat_num: Int = 4): Boolean = {
    val p = GPSParams(sat_num)
    implicit val scale = p.scale
    val model = MatrixTransposeModel(a)

    test(new MatrixTranspose(p, p.rows, p.cols)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.in.ready.poke(true.B)
      for (r <- 0 until p.rows) {
        for (c <- 0 until p.cols) {
          dut.io.in.bits.a(r)(c).poke(p.fixedToChisel(a(r)(c)))
        }
      }
      
      for (r <- 0 until p.rows) {
        for (c <- 0 until p.cols) {
          assert(dut.io.out.bits(r)(c).peek() == model(r)(c))
        }
      }
    }
    true
  }
  behavior of "MatrixTranspose"
  it should "Tranpose Matrix of size 2x2" in {
    0
  }

}

