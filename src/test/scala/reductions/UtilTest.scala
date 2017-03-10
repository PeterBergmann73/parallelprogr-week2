package reductions

import org.scalatest.FunSpec
import util.Util


/**
  * Created by slava on 06.03.17.
  */
class UtilTest extends FunSpec {

  // TODO - fix the method
//  it("bounds") {
//
//    val size = 32
//    val numTasks = (1 to size).toList
//
//    numTasks.foreach {
//      nt =>
//        val calculated = Util.bounds(size = size, numTasks = nt)
//        val c0 = calculated.size
//        assert(c0 == nt, s"number of calculated bounds $c0 is not equal to the number of tasks $nt")
//    }
//  }


  it("countChange") {
    val coins = List(1, 2, 3, 5, 10, 20, 50, 100)

    // map from money to change count
    val target: List[(Int, Int)] = List(
      -1 -> 0,
      0 -> 1,
      1 -> 1,
      2 -> 2,
      3 -> 3,
      4 -> 4
    )

    val counted: List[(Int, Int)] = target.map {
      case (k, v) => k -> ParallelCountChange.countChange(k, coins)
    }

    assert(target === counted)
  }

  it("parenthesses balancing") {
    val s1 = "(if (zero? x) max (/ 1 x))"
    assert(ParallelParenthesesBalancing.balance(s1.toCharArray))

    val s2 = "I told him (that it's not (yet) done). (But he wasn't listening)"
    assert(ParallelParenthesesBalancing.balance(s2.toCharArray))

    val s3 = "(o_()"
    assert(!ParallelParenthesesBalancing.balance(s3.toCharArray))

    val s4 = ":-)"
    assert(!ParallelParenthesesBalancing.balance(s4.toCharArray))

    val s5 = "())("
    assert(!ParallelParenthesesBalancing.balance(s5.toCharArray))
  }


  it("parenthesses parallel balancing") {
    val threshold = 3

    val s1 = "(if (zero? x) max (/ 1 x))"
    assert(ParallelParenthesesBalancing.parBalance(s1.toCharArray, 2 * threshold))

//    val s2 = "I told him (that it's not (yet) done). (But he wasn't listening)"
//    assert(ParallelParenthesesBalancing.parBalance(s2.toCharArray, threshold))
//
//    val s3 = "(o_()"
//    assert(!ParallelParenthesesBalancing.parBalance(s3.toCharArray, threshold))
//
//    val s4 = ":-)"
//    assert(!ParallelParenthesesBalancing.parBalance(s4.toCharArray, threshold))
//
//    val s5 = "())("
//    assert(!ParallelParenthesesBalancing.parBalance(s5.toCharArray, threshold))

  }
}
