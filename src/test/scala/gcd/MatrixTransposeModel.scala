package gps


object MatrixTransposeModel {
  type Matrix = Seq[Seq[Int]]

  def apply(a: Matrix): Matrix = {
    val c = Seq.tabulate(a.head.size, a.size ) {
      (i,j) => a(j)(i)
    }
    c
  }
}
