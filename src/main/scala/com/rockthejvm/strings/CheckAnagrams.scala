package com.rockthejvm.strings

import CountCharacters._

object CheckAnagrams {
  // Given two strings, determine if they contain the same letters
  def checkAnagrams(s1: String, s2: String): Boolean =
    countCharacters(s1.toLowerCase) == countCharacters(s2.toLowerCase)

  def checkAnagrams_v2(s1: String, s2: String): Boolean =
    s1.toLowerCase.sorted == s2.toLowerCase.sorted

  def main(args: Array[String]): Unit = {
    println(checkAnagrams("Scala", "Haskell")) // false
    println(checkAnagrams("Stressed", "Desserts")) // true
    println(checkAnagrams_v2("Stressed", "Desserts")) // true
  }
}