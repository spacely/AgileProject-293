package gps

import chisel3._
import chisel3.util._
import chisel3.experimental

import spire._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}

// FixedPoint refers to Spire fixed point (scala type)
// experimental.FixedPoint refers to chisel type
case class GPSParams(sat_num: Int = 4, width: Int = 8, bp: Int = 4) {
  val rows: Int = sat_num
  val cols: Int = 4


  implicit val scale = FixedScale(1 << bp)

  def fixedToChisel(a: FixedPoint, width: Int = width, bp: Int = bp)(implicit scale: FixedScale): experimental.FixedPoint = {
    //val rational = a.toRational
    //val whole = a.floor.toInt.U(width.W)
    //val frac = (rational.numerator % rational.denominator).toInt.U(bp.W)
    val chiselFixed = experimental.FixedPoint(a.toBigInt, width.W, bp.BP)
    //val chiselFixedWire = Wire(experimental.FixedPoint(width.W, bp.BP))
    //chiselFixedWire := whole ## frac
    //chiselFixed(3,0) := frac 
    chiselFixed
  }
}
