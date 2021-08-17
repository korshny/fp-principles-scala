package recfun

import java.util.stream.Collectors
import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
//  @tailrec
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
//    println("Chars:")
//    chars.map(println)
    val parentheses: List[Char] = chars.filter(p => p.equals('(') || p.equals(')'))
//    println("Parentheses:")
//    parentheses.map(println)
    def go(chars: List[Char], count: Int): Boolean = {
      if (count < 0) false else chars match {
        case Nil => count == 0
        case '(' :: t => go(t, count + 1)
        case ')' :: t => go(t, count - 1)
        case _ => false // Coud it happen?
      }
    }
    if (parentheses.isEmpty) true else go(parentheses, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def go(ballance: Int, coins: List[Int], combo: List[Int]): Int = {

      if ballance.equals(0) then
//        println("Combo: " + combo)
        1
      else if ballance > 0 then coins.map(c =>
        if (!combo.isEmpty && c < combo.head) then 0
        else go(ballance - c, coins, c :: combo)
      ).sum
      else 0
    }

    go(money, coins.sorted, List.empty)
  }
