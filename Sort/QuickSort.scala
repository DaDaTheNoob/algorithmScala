object QuickSort {
  def quickSort(A: Array[Int]): Array[Int] = {
    if(A.length <= 1) A
    else {
      val (left, right) = A.tail.partition(x => x <= A.head)
      quickSort(left) ++ (A.head +: quickSort(right))
    }
  }
}