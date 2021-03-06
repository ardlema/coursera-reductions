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
  ) withWarmer(new Warmer.Default)

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

  /** Returns `true` if the parentheses in the input `chars` are balanced.
   */
  //NEED TO BE IMPROVED IN TERMS OF PERF. WHERE IS THE BOTTLENECK??? CHARS.SIZE???
  def balance(chars: Array[Char]): Boolean = {
    def isBalanced(chars: Array[Char], acc: Int): Boolean = {
      if (acc < 0) false
      else {
        chars.size match {
          case 0 => return acc == 0
          case _ if (chars(0).equals('(')) => isBalanced(chars.tail, acc + 1)
          case _ if (chars(0).equals(')')) => isBalanced(chars.tail, acc - 1)
          case _ => isBalanced(chars.tail, acc)
        }
      }
    }
    if (chars.isEmpty) true
    else isBalanced(chars, 0)
  }

  /** Returns `true` if the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      idx match {
        case x if (x == until) => (arg1, arg2)
        case _ if (chars(idx).equals('(')) => traverse(idx + 1, until, arg1 + 1, arg2)
        case _ if (chars(idx).equals(')')) =>
          if (arg1 > 0) traverse(idx + 1, until, arg1 - 1, arg2)
          else traverse(idx + 1, until, arg1, arg2 + 1)
        case _ => traverse(idx + 1, until, arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val elements = until - from
      if (elements <= threshold) traverse(from, until, 0, 0)
      else {
        val halfSize = elements / 2
        val ((l1, r1), (l2, r2)) = parallel(
          reduce(from, from + halfSize),
          reduce(from + halfSize, until))
        if (l1 > r2) {
          (l1 - r2 + l2) -> r1
        } else {
          l2 -> (r2 - l1 + r1)
        }
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
