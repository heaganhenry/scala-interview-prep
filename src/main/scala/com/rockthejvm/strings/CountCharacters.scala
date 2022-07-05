package com.rockthejvm.strings

import scala.annotation.tailrec

object CountCharacters {
  // Given a string, return the number of occurrences of each char in a Map
  def countCharacters(s: String): Map[Char, Int] = {
    @tailrec
    def countCharsTailrec(i: Int, acc: Map[Char, Int]): Map[Char, Int] = {
      if (i == s.length) acc
      else countCharsTailrec(i + 1, acc + (s.charAt(i) -> (acc(s.charAt(i)) + 1)))
    }

    countCharsTailrec(0, Map().withDefaultValue(0))
  }

  def main(args: Array[String]): Unit = {
    println(countCharacters("Scala")) // Map(S -> 1, c -> 1, a -> 2, l -> 1)
  }
}