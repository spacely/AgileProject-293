package gps
import chisel3._
import chiseltest.{iotesters, defaults, ChiselScalatestTester, WriteVcdAnnotation, testableFixedPoint, ValidDriver, DecoupledDriver, testableBool, testableClock}
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental

import spire._
import spire.math._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}

import gps.MatrixTransposeModel.Matrix

object MatMulData {
  val p = GPSParams()
  implicit val scale = p.scale

  val in2x4  = Seq(Seq(FixedPoint(1.0),FixedPoint(2.0),FixedPoint(3.0),FixedPoint(4.0)),
                   Seq(FixedPoint(5.0),FixedPoint(6.0),FixedPoint(7.0),FixedPoint(8.0)))
  val in4x2  = Seq(Seq(FixedPoint(1.0),FixedPoint(2.0)),
                   Seq(FixedPoint(3.0),FixedPoint(4.0)),
                   Seq(FixedPoint(5.0),FixedPoint(6.0)),
                   Seq(FixedPoint(7.0),FixedPoint(8.0)))
  val out2x2 = Seq(Seq(FixedPoint(50.0), FixedPoint(60.0)),
                   Seq(FixedPoint(114.0),FixedPoint(140.0)))
  val out4x4 = Seq(Seq(FixedPoint(11.0), FixedPoint(14.0), FixedPoint(17.0), FixedPoint(20.0)),
                   Seq(FixedPoint(23.0), FixedPoint(30.0), FixedPoint(37), FixedPoint(44)),
                   Seq(FixedPoint(35.0), FixedPoint(46.0), FixedPoint(57.0), FixedPoint(68.0)),
                   Seq(FixedPoint(47.0), FixedPoint(62.0), FixedPoint(77.0), FixedPoint(92.0)))
}

class MatMulModelTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MatMulModel"

  it should "multiply in2x4 x in4x2" in {
    assert(MatMulModel(MatMulData.p, MatMulData.in2x4, MatMulData.in4x2) == MatMulData.out2x2)
  }

  it should "multiply in4x2 x in2x4" in {
    assert(MatMulModel(MatMulData.p, MatMulData.in4x2, MatMulData.in2x4) == MatMulData.out4x4)
  }
}

class MatrixMulTester extends AnyFlatSpec with ChiselScalatestTester {
  def doMatMulSCTest(a: Matrix, b: Matrix, parallelism: Int): Unit = {
    test(new MatrixMul(MatMulData.p, 1, parallelism, a.size, b.size, a.size, a(0).size, b(0).size, b(0).size, a.size * a(0).size, b.size * b(0).size, a.size * b(0).size)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // load input matrices
      implicit val scale = MatMulData.p.scale
      dut.io.in.valid.poke(true.B)
      dut.io.in.ready.expect(true.B)
      // OK for dut.io.out.valid to be true with junk data
      for (r <- 0 until a.size) {
        for (c <- 0 until a(0).size) {
          dut.io.in.bits.a(r)(c).poke(MatMulData.p.fixedToChisel(a(r)(c)))
        }
      }
      for (r <- 0 until b.size) {
        for (c <- 0 until b(0).size) {
          dut.io.in.bits.b(r)(c).poke(MatMulData.p.fixedToChisel(b(r)(c)))
        }
      }
      // println("-----")
      // println(dut.io.state.peek())
      // println(dut.io.counter.peek())
      // println(dut.io.in.ready.peek())
      // println(dut.io.out.valid.peek())
      // println("\n")
      dut.clock.step()
      // println(dut.io.state.peek())
      // println(dut.io.counter.peek())
      // println(dut.io.in.ready.peek())
      // println(dut.io.out.valid.peek())

      // println("\n")
      // wait for completion
      dut.io.in.ready.expect(false.B)
      dut.io.out.valid.expect(false.B)
      dut.clock.step(a.size * b.size * b(0).size / parallelism)
      // println(dut.io.state.peek())
      // println(dut.io.counter.peek())
      // println(dut.io.in.ready.peek())
      // println(dut.io.out.valid.peek())
      // println("\n")
      // check for completion & result
      dut.io.in.ready.expect(true.B)
      dut.io.out.valid.expect(true.B)
      val expected = MatMulModel(MatMulData.p, a, b)
      for (r <- 0 until a.size) {
        for (c <- 0 until b(0).size) {
          dut.io.out.bits(r)(c).expect(MatMulData.p.fixedToChisel(expected(r)(c)))
        }
      }
    }
  }

    behavior of "MatrixMul"
  it should "multiply in4x2 x in2x4 (no parallelism)" in {
    doMatMulSCTest(MatMulData.in4x2, MatMulData.in2x4,1)
  }

  it should "multiply in4x2 x in2x4 (full parallelism)" in {
    doMatMulSCTest(MatMulData.in4x2, MatMulData.in2x4,4)
  }
}