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

  it should "Convert spire fixed point to chisel (4.0 -> 4.0, 0/4 frac persicion)" in {
      val p = GPSParams(width = 8, bp = 4)
      implicit val scale = p.scale
      
      val double = 4.0
      val spire_num = FixedPoint(double)
      test(new FixedToChiselModel(spire_num, p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        dut.io.out.expect(experimental.FixedPoint.fromDouble(spire_num.toDouble, p.width.W, p.bp.BP))
      assert(dut.io.out.peekDouble() == double)
      assert(spire_num.toDouble == double)
      
    }
  }

    it should "Convert spire fixed point to chisel (4.5 -> 4.5,  1/4 frac persicion)" in {
      val p = GPSParams(width = 8, bp = 4)
      implicit val scale = p.scale
      
      val double = 4.5
      val spire_num = FixedPoint(double)

      test(new FixedToChiselModel(spire_num, p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        dut.io.out.expect(experimental.FixedPoint.fromDouble(spire_num.toDouble, p.width.W, p.bp.BP))
      assert(dut.io.out.peekDouble() == double)
      assert(spire_num.toDouble == double)
    }
  }

    it should "Convert spire fixed point to chisel (4.75 -> 4.75, 2/4 frac persicion)" in {
      val p = GPSParams(width = 8, bp = 4)
      implicit val scale = p.scale
      
      val double = 4.75
      val spire_num = FixedPoint(double)
      test(new FixedToChiselModel(spire_num, p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        dut.io.out.expect(experimental.FixedPoint.fromDouble(spire_num.toDouble, p.width.W, p.bp.BP))
      assert(dut.io.out.peekDouble() == double)
      assert(spire_num.toDouble == double)
    }
  }
    it should "Convert spire fixed point to chisel (4.875 -> 4.875, 3/4 frac persicion)" in {
      val p = GPSParams(width = 8, bp = 4)
      implicit val scale = p.scale
      
      val double = 4.875
      val spire_num = FixedPoint(double)
      test(new FixedToChiselModel(spire_num, p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        dut.io.out.expect(experimental.FixedPoint.fromDouble(spire_num.toDouble, p.width.W, p.bp.BP))
      assert(dut.io.out.peekDouble() == double)
      assert(spire_num.toDouble == double)
    }
  }

    it should "Convert spire fixed point to chisel (4.9375 -> 4.9375, 4/4 frac persicion)" in {
      val p = GPSParams(width = 8, bp = 4)
      implicit val scale = p.scale
      
      val double = 4.9375
      val spire_num = FixedPoint(double)
      test(new FixedToChiselModel(spire_num, p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        dut.io.out.expect(experimental.FixedPoint.fromDouble(spire_num.toDouble, p.width.W, p.bp.BP))
      assert(dut.io.out.peekDouble() == double)
      assert(spire_num.toDouble == double)
    }
  }
    it should "Convert spire fixed point to chisel (4.96875 -> 4.9375, 5/4 frac persicion)" in {
      val p = GPSParams(width = 8, bp = 4)
      implicit val scale = p.scale
      
      val double = 4.96875
      val spire_num = FixedPoint(double)
      test(new FixedToChiselModel(spire_num, p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        dut.io.out.expect(experimental.FixedPoint.fromDouble(spire_num.toDouble, p.width.W, p.bp.BP))
      assert(dut.io.out.peekDouble() != double)
      assert(spire_num.toDouble != double)
    }
  }
}