package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
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

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {

    val length = chars.length

    @scala.annotation.tailrec
    def check(from: Int, cum: Int): Boolean = {
      if (cum < 0) {
        false
      } else if (from >= length) {
        cum == 0
      } else {
        val h = chars(from)

        val current = h match {
          case '(' => 1
          case ')' => -1
          case _ => 0
        }

        check(from + 1, cum + current)
      }
    }

    if (length == 0) {
      true
    } else {
      check(0, 0)
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    println(s"chars: ${chars.toList}")
    require(threshold >= 1, s"illegal threshold $threshold")

    // first int - number of unbalanced left parenthesis
    // second int - number of unbalanced right parenthesis
    @scala.annotation.tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      println(s"Entering threshold, idx $idx, until $until")
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

        traverse(idx = idx + 1, until = until, arg1 = a1, arg2 = a2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      println(s"Entering reduce, from: $from, until: $until")
      if ((until - from) <= threshold) {
        traverse(idx = from, until = until, 0, 0)
      } else {
        println(s"calculating mid, from $from, until $until")
        val mid = (until - from) / 2

        println(s"mid $mid")

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
