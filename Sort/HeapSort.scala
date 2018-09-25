def switch(A: Array[Int], a: Int, b: Int): Array[Int] = {
  val mid = A(a)
  A(a) = A(b)
  A(b) = mid
  A
}

def maxHeapify(A: Array[Int], index: Int, len: Int): Array[Int] = {
  val left = 2 * index + 1
  val right = 2 * index + 2
  if(left < len && A(left) > A(index)) {
    if(right < len && A(right) >= A(left)) {
      switch(A, right, index)
      maxHeapify(A, right, len)
    }
    else {
      switch(A, left, index)
      maxHeapify(A, left, len)
    }
  }
  else if(right < len && A(right) > A(index)) {
    switch(A, right, index)
    maxHeapify(A, right, len)
  }
  else A
}

def maxHeap(A: Array[Int], len: Int): Array[Int] = {
  for(i <- (0 until len / 2).reverse) {
    maxHeapify(A, A(i), len)
  }
  A
}

def heapSort(A: Array[Int], len: Int): Array[Int] = {
  maxHeap(A, len)
  for(i <- 0 until len) {
    switch(A, 0, len - i - 1)
    maxHeap(A, len - i - 1)
  }
  A
}

val A = Array(3, 8, 7, 6, 5, 2, 4, 1, 9)
val len = A.length
maxHeap(A, len)

for(i <- 0 until len) {
  switch(A, 0, len - i - 1)
  for(k <- A) print(k+",")
  print("\n")
  maxHeap(A, len - i - 1)
}
maxHeap(Array(3,8,7,6,5,2,4,1,9), 8)