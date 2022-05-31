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

  // append another list
  def ++[S >: T](anotherList: RList[S]): RList[S]

  // remove an element at a given index
  def removeAt(index: Int): RList[T]
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
  override def reverse: RList[Nothing] = RNil

  // append another list
  def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  // remove an element at a given index
  def removeAt(index: Int): RList[Nothing] = RNil
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
    def applyTailrec(remList: RList[T], curIndex: Int): T =
      if (index == curIndex) remList.head
      else applyTailrec(remList.tail, curIndex + 1)

    if (index < 0) throw new NoSuchElementException
    else applyTailrec(this, 0)
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
    def lengthTailrec(remList: RList[T], count: Int): Int =
      if (remList.isEmpty) count
      else lengthTailrec(remList.tail, count + 1)

    lengthTailrec(this, 0)
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

  // append another list
  def ++[S >: T](anotherList: RList[S]): RList[S] = {
    /*
    [1,2,3] ++ [4,5] = concatTailrec([4,5], [3,2,1]).reverse
    = concatTailrec([5], [4,3,2,1]).reverse
    = concatTailrec([], [5,4,3,2,1]).reverse
    = [1,2,3,4,5]

    Complexity: O(M + N)
    length of this list = N
    length of the other list = M
    */
    @tailrec
    def concatTailrec(remList: RList[S], accList: RList[S]): RList[S] =
      if (remList.isEmpty) accList
      else concatTailrec(remList.tail, remList.head :: accList)

    concatTailrec(anotherList, this.reverse).reverse
  }

  // remove an element at a given index
  def removeAt(index: Int): RList[T] = {
    /*
    [1,2,3,4,5].removeAt(2) = removeAtTailrec([1,2,3,4,5], RNil, 0)
    = removeAtTailrec([2,3,4,5], [1], 1)
    = removeAtTailrec([3,4,5], [2,1], 2)
    = [2,1].reverse ++ [4,5]
    = [1,2,4,5]

    Complexity: O(N)
    */
    @tailrec
    def removeAtTailrec(remList: RList[T], accList: RList[T], curIndex: Int): RList[T] = {
      if (remList.isEmpty) accList.reverse
      else if (index == curIndex) accList.reverse ++ remList.tail
      else removeAtTailrec(remList.tail, remList.head :: accList, curIndex + 1)
    }

    if (index <= 0) this
    else removeAtTailrec(this, RNil, 0)
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def fromTailrec(remIterable: Iterable[T], accList: RList[T]): RList[T] =
      if (remIterable.isEmpty) accList
      else fromTailrec(remIterable.tail, remIterable.head :: accList)

    fromTailrec(iterable, RNil).reverse
  }
}

object ListProblems {

  def main(args: Array[String]): Unit = {
    val aSmallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
    val aLargeList = RList.from(1 to 10000)

    // test get-kth
    println(aSmallList(0))
    println(aLargeList(8500))

    // test length
    println(aSmallList.length)
    println(aLargeList.length)

    // test reverse
    println(aSmallList.reverse)
    println(aLargeList.reverse)

    // test concat
    println(aSmallList ++ aLargeList)

    // test removeAt
    println(aLargeList.removeAt(13))
  }
}