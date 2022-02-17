package gps


object MatrixInverseModel {
  type Matrix = Seq[Seq[Float]]
  def genIdentity(n: Int): Matrix = Seq.tabulate(n,n) { (i,j) => if (i==j) 1 else 0 }
  def append(a: Matrix, b: Matrix): Matrix = {
      assert(a.size == b.size)
      asse(a.head.size == b.head.size)
      val c = Seq.tabulate(a.head.size, a.size + b.size) {
          (i,j) => if(j < a.size) a(i,j)
                   if(j > a.size) b(i,j-a.size)
      }
      c
  }
  def inv(a: Matrix): Matrix = {
    assert(a.size == a.head.size)

    a
  }
}
