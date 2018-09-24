object MergeSort {
  def merge(X: List[Int], Y: List[Int]): List[Int] = {
    (X, Y) match {
      case (_, Nil) => X
      case (Nil, _) => Y
      case _ =>
        if (X.head<=Y.head) X.head::merge(X.tail, Y)
        else Y.head::merge(X, Y.tail)
    }
  }
  def splitList(A: List[Int]): (List[Int], List[Int]) = {
    val l = A.length / 2
    val res = A.splitAt(l)
    res
  }
  def mergeSort(A: List[Int]): List[Int] = {
    if (A.length == 1) A
    else {
      val X = splitList(A)._1
      val Y = splitList(A)._2
      merge(mergeSort(X), mergeSort(Y))
    }
  }
}