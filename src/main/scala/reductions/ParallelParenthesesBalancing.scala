package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  // changed values because it took too long
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10, // was 40
    Key.exec.maxWarmupRuns -> 20, // was 80
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}


object ParallelParenthesesBalancing {


  // first int - number of unbalanced left parenthesis
  // second int - number of unbalanced right parenthesis
  @scala.annotation.tailrec
  def traverse(chars: Array[Char], idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
    //println(s"Entering threshold, idx $idx, until $until")
    if (idx >= until) {
      (arg1, arg2)
    } else {
      val (a1, a2) = chars(idx) match {
        case '(' => (arg1 + 1, arg2)
        case ')' =>
          if (arg1 > 0) {
            (arg1 - 1, arg2)
          } else {
            (arg1, arg2 + 1)
          }
        case _ => (arg1, arg2)
      }

      traverse(chars: Array[Char], idx = idx + 1, until = until, arg1 = a1, arg2 = a2)
    }
  }


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    traverse(chars, 0, chars.length, 0, 0) == (0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    require(threshold >= 1, s"illegal threshold $threshold")

    def reduce(from: Int, until: Int): (Int, Int) = {

      if ((until - from) <= threshold) {
        traverse(chars, idx = from, until = until, 0, 0)
      } else {
        val mid = (until + from) / 2

        val (l, r) = parallel(reduce(from = from, until = mid), reduce(from = mid, until = until))

        val matched = scala.math.min(l._1, r._2)
        (l._1 + r._1 - matched, l._2 + r._2 - matched)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
