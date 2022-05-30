package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  // standard functions
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  // get element at kth index
  def apply(index: Int): T

  // get the size of the list
  def length: Int

  // reverse the list
  def reverse: RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  // get element at kth index
  override def apply(index: Int): Nothing = throw new NoSuchElementException

  // get the size of the list
  override def length: Int = 0

  // reverse the list
  override def reverse: RList[Nothing] = this
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String =
      if (remaining.isEmpty) result
      else toStringTailrec(remaining.tail, s"$result, ${remaining.head}")

    s"[${toStringTailrec(tail, s"$head")}]"
  }

  // get element at kth index
  override def apply(index: Int): T = {
    /*
    [1,2,3,4,5].apply(2) = applyTailrec([1,2,3,4,5], 0)
    = applyTailrec([2,3,4,5], 1)
    = applyTailrec([3,4,5], 2)
    = 3

    Complexity: O(min(N, index))
    */
    @tailrec
    def applyTailrec(remList: RList[T], curIndex: Int = 0): T =
      if (index == curIndex) remList.head
      else applyTailrec(remList.tail, curIndex + 1)

    if (index < 0) throw new NoSuchElementException
    else applyTailrec(this)
  }

  // get the size of the list
  override def length: Int = {
    /*
    [1,2,3,4,5].length = lengthTailrec([1,2,3,4,5], 0)
    = lengthTailrec([2,3,4,5], 1)
    = lengthTailrec([3,4,5], 2)
    = lengthTailrec([4,5], 3)
    = lengthTailrec([5], 4)
    = lengthTailrec([], 5)
    = 5

    Complexity: O(N)
    */
    @tailrec
    def lengthTailrec(remList: RList[T], count: Int = 0): Int =
      if (remList.isEmpty) count
      else lengthTailrec(remList.tail, count + 1)

    lengthTailrec(this)
  }

  // reverse the list
  override def reverse: RList[T] = {
    /*
    [1,2,3,4,5].reverse = reverseTailrec([1,2,3,4,5], RNil)
    = reverseTailrec([2,3,4,5], [1])
    = reverseTailrec([3,4,5], [2,1])
    = reverseTailrec([4,5], [3,2,1])
    = reverseTailrec([5], [4,3,2,1])
    = reverseTailrec([], [5,4,3,2,1])
    = [5,4,3,2,1]

    Complexity: O(N)
    */
    @tailrec
    def reverseTailrec(remList: RList[T], accList: RList[T]): RList[T] =
      if (remList.isEmpty) accList
      else reverseTailrec(remList.tail, remList.head :: accList)

    reverseTailrec(this, RNil)
  }
}

object ListProblems {

  def main(args: Array[String]): Unit = {
    val aSmallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)

    // test get-kth
    println(aSmallList(2))

    // test length
    println(aSmallList.length)

    // test reverse
    println(aSmallList.reverse)
  }
}