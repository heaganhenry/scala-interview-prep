package com.rockthejvm.strings

object RansomNote {
  // Given two strings, note and magazine, write a function that confirms if enough characters are available in the magazine to compose the note
  def ransomNote(note: String, magazine: String): Boolean = {
    def buildMap(string: String) =
      string.groupBy(identity).view.mapValues(_.length).toMap

    val noteMap = buildMap(note)
    val magazineMap = buildMap(magazine)
    noteMap.forall((k, v) => v <= magazineMap.getOrElse(k, 0))
  }

  def main(args: Array[String]): Unit = {
    println(ransomNote("Hello", "Heal the world")) // true
    println(ransomNote("Hello", "Buy our products")) // false
  }
}