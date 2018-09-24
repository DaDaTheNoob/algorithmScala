object FindMaximunSubarray_dc {
  def sumArray(a: Int, b: Int): Int = a + b
  def findCrossingSubarray(A: Array[Int]): (Array[Int], Int) = {
    val mid = A.length / 2
    val (leftSum, left) = A.splitAt(mid)._1.scanRight(0)(sumArray).dropRight(1).zipWithIndex.max
    val (rightSum, right) = A.splitAt(mid)._2.scanLeft(0)(sumArray).tail.zipWithIndex.max
    (A.slice(left, right + 1 + mid), leftSum + rightSum)
  }
  def findMaximunSubarray(A: Array[Int]): (Array[Int], Int) = {
    if(A.length > 1) {
      val mid = A.length / 2
      val midRes = findCrossingSubarray(A)
      val leftRes = findMaximunSubarray(A.splitAt(mid)._1)
      val rightRes = findMaximunSubarray(A.splitAt(mid)._2)
      if (midRes._2 >= leftRes._2 && midRes._2 >= rightRes._2) midRes
      else if (leftRes._2 >= midRes._2 && leftRes._2 >= rightRes._2) leftRes
      else rightRes
    }
    else (A, A(0))
  }
}