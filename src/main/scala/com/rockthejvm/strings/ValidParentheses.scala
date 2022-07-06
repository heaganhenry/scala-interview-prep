package com.rockthejvm.strings

import scala.annotation.tailrec

object ValidParentheses {
  // Given a string of parentheses, determine if each open brace is followed by a corresponding closing brace
  def hasValidParentheses(string: String): Boolean = {
    val closeToOpen = Map(')' -> '(')

    @tailrec
    def validParensTailrec(remaining: String, stack: List[Char]): Boolean = {
      if (remaining.isEmpty) stack.isEmpty
      else if (stack.nonEmpty && closeToOpen.contains(remaining.head) && stack.head == closeToOpen(remaining.head)) {
        validParensTailrec(remaining.tail, stack.tail)
      } else validParensTailrec(remaining.tail, remaining.head :: stack)
    }

    validParensTailrec(string, List())
  }

  def main(args: Array[String]): Unit = {
    println(hasValidParentheses("()")) // true
    println(hasValidParentheses("()()")) // true
    println(hasValidParentheses("(())")) // true
    println(hasValidParentheses(")(")) // false
  }
}