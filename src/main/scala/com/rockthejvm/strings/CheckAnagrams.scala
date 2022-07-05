package com.rockthejvm.strings

import scala.annotation.tailrec

object CheckAnagrams {
  // Given two strings, determine if they contain the same letters
  def checkAnagrams(s1: String, s2: String): Boolean = {
    @tailrec
    def countCharsTailrec(s: String, i: Int, acc: Map[Char, Int]): Map[Char, Int] = {
      if (i == s.length) acc
      else countCharsTailrec(s, i + 1, acc + (s.charAt(i) -> (acc(s.charAt(i)) + 1)))
    }

    val s1CharMap = countCharsTailrec(s1.toLowerCase(), 0, Map().withDefaultValue(0))
    val s2CharMap = countCharsTailrec(s2.toLowerCase(), 0, Map().withDefaultValue(0))
    s1CharMap == s2CharMap
  }

  def checkAnagrams_v2(s1: String, s2: String): Boolean = s1.sorted equalsIgnoreCase s2.sorted

  def main(args: Array[String]): Unit = {
    println(checkAnagrams("Scala", "Haskell")) // false
    println(checkAnagrams("Stressed", "Desserts")) // true
    println(checkAnagrams_v2("Stressed", "Desserts")) // true
  }
}