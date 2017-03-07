package util



/**
  * Created by slava on 06.03.17.
  */
object Util {


  // the function assumes that the list is sorted
  def go(left: Int, c0: List[Int], count: Int): Int = {

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
            // just to reduce number recursive calls
            count + 1
          } else {
            val (r1, r2) = (go(left, t, count), go(left - h, c0, count))
            r1 + r2
          }
      }
    }
  }


}
