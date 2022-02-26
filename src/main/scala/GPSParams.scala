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


  implicit val scale = FixedScale(1 << (bp-1))

  def fixedToChisel(a: FixedPoint, width: Int = width, bp: Int = bp)(implicit scale: FixedScale): experimental.FixedPoint = {
    val chiselFixed = experimental.FixedPoint.fromDouble(a.toDouble, width.W, bp.BP)
    chiselFixed
  }
}
