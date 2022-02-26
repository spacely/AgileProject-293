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

class FixedPointTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "fixedToChisel"

  it should "Convert spire fixed point to chisel (d = 4.8, w = 8, bp = 4)" in {
      val p = GPSParams(width = 8, bp = 4)
      implicit val scale = p.scale
      val double = 4.8
      val spire_num = FixedPoint(double)
      test(new FixedToChiselModel(spire_num, p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        dut.io.out.expect(experimental.FixedPoint.fromDouble(double, p.width.W, p.bp.BP))
    }
  }
}