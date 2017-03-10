package util


/**
  * Created by slava on 06.03.17.
  */
object Util {


  // the method is not using any build-in functions to make as memory efficient as possible
  // to avoid OOM exceptions that we get in parallel cases.
  // TODO - the method is not working correctly
  def bounds(size: Int, numTasks: Int): List[(Int, Int)] = {

    val step: Int = scala.math.min(1, size / numTasks)

    var where = 0

    val res: List[(Int, Int)] = {
      val temp = Array.tabulate(numTasks + 1)(_ * step)
      val lastInd = numTasks - 1
      val last = temp(lastInd)

      require(last <= size, s"Last element $last is bigger than the array size $size")

      // ensure that the last element is equal to the array size
      if (last < size) {
        temp(lastInd) = numTasks
      }

      val l = temp.toList
      l.zip(l.tail)
    }

    println(res)

    res
  }


  // the function assumes that the list is sorted
  def countChange(left: Int, c0: List[Int], count: Int): Int = {

    if (left == 0) {
      count + 1
    } else if (left < 0) {
      count
    } else {
      c0 match {
        case Nil => count
        case h :: t =>
          if (h > left) {
            count
          } else if (h == left) {
            // this case will be handled below when subtracting the head,
            // however, let us handled it separately
            // just to reduce the number of recursive calls
            count + 1
          } else {
            val (r1, r2) = (countChange(left, t, count), countChange(left - h, c0, count))
            r1 + r2
          }
      }
    }
  }


}
