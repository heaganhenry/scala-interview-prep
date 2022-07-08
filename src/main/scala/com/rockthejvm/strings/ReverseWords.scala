package com.rockthejvm.strings

object ReverseWords {
  // Given a string return a result string with the order of each word reversed.
  // You are also expected to remove any additional blank spaces, e.g. "  hello  world  " -> "world hello"
  def reverseWords(string: String): String =
    string.split(" ").filterNot(_.isEmpty).reverse.mkString(" ")

  def main(args: Array[String]): Unit = {
    println(reverseWords("Alice loves Scala")) // "Scala loves Alive"
    println(reverseWords("  hello   world  ")) // "world hello"
  }
}