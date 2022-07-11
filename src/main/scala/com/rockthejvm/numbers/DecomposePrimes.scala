package com.rockthejvm.numbers

import scala.annotation.tailrec

object DecomposePrimes {
  // Given a number n, return a list of it's constituent prime divisors
  def decompose(n: Int): List[Int] = {
    assert( n >= 0)

    @tailrec
    def decomposeTailrec(remaining: Int, currentDivisor: Int, accumulator: List[Int]): List[Int] = {
      if (currentDivisor > Math.sqrt(remaining)) remaining :: accumulator
      else if (remaining % currentDivisor == 0) decomposeTailrec(remaining / currentDivisor, currentDivisor, currentDivisor :: accumulator)
      else decomposeTailrec(remaining, currentDivisor + 1, accumulator)
    }

    decomposeTailrec(n, 2, List())
  }

  def main(args: Array[String]): Unit = {
    println(decompose(11)) // [11]
    println(decompose(15)) // [5,3]
    println(decompose(16)) // [2,2,2,2]
  }
}
