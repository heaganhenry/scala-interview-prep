package com.rockthejvm.lists

import scala.annotation.tailrec
import scala.util.Random

sealed abstract class RList[+T] {
  // standard functions
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  /**
    * Easy problems
    */
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

  // map, flatMap, filter
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  /**
    * Medium problems
    */
  // run-length encoding
  def rle: RList[(T, Int)]

  // duplicate each element k times
  def duplicateEach(k: Int): RList[T]

  // rotation by k positions to the left
  def rotate(k: Int): RList[T]

  // random sample
  def sample(k: Int): RList[T]

  /**
    * Hard problems
    */
  // sort the list in the order defined by the Ordering object
  def insertionSort[S >: T](ordering: Ordering[S]): RList[S]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  /**
    * Easy problems
    */
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

  // map, flatMap, filter
  override def map[S](f: Nothing => S): RList[S] = RNil
  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil
  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  /**
    * Medium problems
    */
  // run-length encoding
  override def rle: RList[(Nothing, Int)] = RNil

  // duplicate each element k times
  override def duplicateEach(k: Int): RList[Nothing] = RNil

  // rotation by k positions to the left
  override def rotate(k: Int): RList[Nothing] = RNil

  // random sample
  override def sample(k: Int): RList[Nothing] = RNil

  /**
    * Hard problems
    */
  // sort the list in the order defined by the Ordering object
  override def insertionSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], accumulator: String): String = {
      if (remaining.isEmpty) accumulator
      else toStringTailrec(remaining.tail, s"$accumulator, ${remaining.head}")
    }

    s"[${toStringTailrec(tail, s"$head")}]"
  }

  /**
    * Easy problems
    */
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
    def applyTailrec(remaining: RList[T], curIndex: Int): T = {
      if (index == curIndex) remaining.head
      else applyTailrec(remaining.tail, curIndex + 1)
    }

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
    def lengthTailrec(remaining: RList[T], count: Int): Int = {
      if (remaining.isEmpty) count
      else lengthTailrec(remaining.tail, count + 1)
    }

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
    def reverseTailrec(remaining: RList[T], accumulator: RList[T]): RList[T] = {
      if (remaining.isEmpty) accumulator
      else reverseTailrec(remaining.tail, remaining.head :: accumulator)
    }

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
    def concatTailrec(otherList: RList[S], thisList: RList[S]): RList[S] = {
      if (otherList.isEmpty) thisList
      else concatTailrec(otherList.tail, otherList.head :: thisList)
    }

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
    def removeAtTailrec(remaining: RList[T], accumulator: RList[T], curIndex: Int): RList[T] = {
      if (remaining.isEmpty) this
      else if (index == curIndex) accumulator.reverse ++ remaining.tail
      else removeAtTailrec(remaining.tail, remaining.head :: accumulator, curIndex + 1)
    }

    if (index <= 0) this
    else removeAtTailrec(this, RNil, 0)
  }

  // map, flatMap, filter
  def map[S](f: T => S): RList[S] = {
    /*
    [1,2,3].map(x => x + 1) = mapTailrec([1,2,3], [])
    = mapTailrec([2,3], [2])
    = mapTailrec([3], [3, 2])
    = mapTailrec([], [4,3,2])
    = [4,3,2].reverse
    = [2,3,4]

    Complexity: O(N)
    */
    @tailrec
    def mapTailrec(remaining: RList[T], accumulator: RList[S]): RList[S] = {
      if (remaining.isEmpty) accumulator.reverse
      else mapTailrec(remaining.tail, f(remaining.head) :: accumulator)
    }

    mapTailrec(this, RNil)
  }

  def flatMap[S](f: T => RList[S]): RList[S] = {
    /*
    [1,2,3].flatMap(x => [x, 2 * x]) = fmTailrec([1,2,3], RNil)
    = fmTailrec([2,3], [1,2].reverse ++ []) = fmTailrec([2,3], [1,2].reverse)
    = fmTailrec([3], [2,4].reverse ++ [1,2].reverse)
    = fmTailrec([], [3,6].reverse ++ [2,4].reverse ++ [1,2].reverse)
    = [6,3,4,2,2,1].reverse
    = [1,2,2,4,3,6]

    Complexity: O(Z^2)
    */
    @tailrec
    def flatMapTailrec(remaining: RList[T], accumulator: RList[S]): RList[S] = {
      if (remaining.isEmpty) accumulator.reverse
      else flatMapTailrec(remaining.tail, f(remaining.head).reverse ++ accumulator)
    }

    /*
    [1,2,3].flatMap(x => [x, 2 * x]) = betterFlatMap([1,2,3], [])
    = betterFlatMap([2,3], [[2,1]])
    = betterFlatMap([3], [[4,2], [2,1]])
    = betterFlatMap([], [[6,3], [4,2], [2,1]])
    = concatenateAll([[6,3], [4,2], [2,1]], [], [])
    = concatenateAll([[4,2], [2,1]], [6,3], [])
    = concatenateAll([[4,2], [2,1]], [3], [6])
    = concatenateAll([[4,2], [2,1]], [], [3,6])
    = concatenateAll([[2,1]], [4,2], [3,6])
    = concatenateAll([[2,1]], [2], [4,3,6])
    = concatenateAll([[2,1]], [], [2,4,3,6])
    = concatenateAll([], [2,1], [2,4,3,6])
    = concatenateAll([], [1], [2,2,4,3,6])
    = concatenateAll([], [], [1,2,2,4,3,6])
    = [1,2,2,4,3,6]

    Complexity: O(N + Z)
    */
    @tailrec
    def betterFlatMap(remaining: RList[T], accumulator: RList[RList[S]]): RList[S] = {
      if (remaining.isEmpty) concatenateAll(accumulator, RNil, RNil)
      else betterFlatMap(remaining.tail, f(remaining.head).reverse :: accumulator)
    }

    /*
    Complexity: O(Z)
    */
    @tailrec
    def concatenateAll(elements: RList[RList[S]], currentList: RList[S], accumulator: RList[S]): RList[S] = {
      if (elements.isEmpty && currentList.isEmpty) accumulator
      else if (currentList.isEmpty) concatenateAll(elements.tail, elements.head, accumulator)
      else concatenateAll(elements, currentList.tail, currentList.head :: accumulator)
    }

    betterFlatMap(this, RNil)
  }

  def filter(f: T => Boolean): RList[T] = {
    /*
    [1,2,3,4,5].filter(x => x % 2 == 0) = filterTailrec([1,2,3,4,5], [])
    = filterTailrec([2,3,4,5], [])
    = filterTailrec([3,4,5], [2])
    = filterTailrec([4,5], [2])
    = filterTailrec([5], [4,2])
    = filterTailrec([], [4,2])
    = [2,4]

    Complexity: O(N)
    */
    @tailrec
    def filterTailrec(remaining: RList[T], accumulator: RList[T]): RList[T] = {
      if (remaining.isEmpty) accumulator.reverse
      else if (f(remaining.head)) filterTailrec(remaining.tail, remaining.head :: accumulator)
      else filterTailrec(remaining.tail, accumulator)
    }

    filterTailrec(this, RNil)
  }

  /**
    * Medium problems
    */
  // run-length encoding
  override def rle: RList[(T, Int)] = {
    /*
    [1,1,1,2,2,3,4,4,4,5].rle = rleTailrec([1,1,2,2,3,4,4,4,5], (1,1), [])
    = rlet([1,2,2,3,4,4,4,5], (1,2), [])
    = rlet([2,2,3,4,4,4,5], (1,3), [])
    = rlet([2,3,4,4,4,5], (2, 1), [(1,3)])
    = rlet([3,4,4,4,5], (2,2), [(1,3)])
    = rlet([4,4,4,5], (3,1), [(2,2), (1,3)]
    = ...
    = [(5,1), (4,3), (3,1), (2,2), (1,3)].reverse

    Complexity: O(N)
    */
    @tailrec
    def rleTailrec(remaining: RList[T], currentTuple: (T, Int), accumulator: RList[(T, Int)]): RList[(T, Int)] = {
      if (remaining.isEmpty) currentTuple :: accumulator
      else if (remaining.head == currentTuple._1) rleTailrec(remaining.tail, currentTuple.copy(_2 = currentTuple._2 + 1), accumulator)
      else rleTailrec(remaining.tail, (remaining.head, 1), currentTuple :: accumulator)
    }

    rleTailrec(tail, (head, 1), RNil).reverse
  }

  // duplicate each element k times
  override def duplicateEach(k: Int): RList[T] = {
    /*
    [1,2].duplicateEach(3) = detr(0, [1,2], [])
    = detr(1, [1,2], [1])
    = detr(2, [1,2], [1,1])
    = detr(3, [1,2], [1,1,1])
    = detr(0, [2], [1,1,1])
    = detr(1, [2], [2,1,1,1])
    = detr(2, [2], [2,2,1,1,1])
    = detr(3, [2], [2,2,2,1,1,1])
    = detr(0, [], [2,2,2,1,1,1])
    = [2,2,2,1,1,1].reverse

    Complexity = O(N * K)
    */
    @tailrec
    def duplicateEachTailrec(i: Int, remaining: RList[T], accumulator: RList[T]): RList[T] = {
      if (remaining.isEmpty) accumulator.reverse
      else if (i == k) duplicateEachTailrec(0, remaining.tail, accumulator)
      else duplicateEachTailrec(i + 1, remaining, remaining.head :: accumulator)
    }

    if (k < 0) throw new IllegalArgumentException
    duplicateEachTailrec(0, this, RNil)
  }

  // rotation by k positions to the left
  override def rotate(k: Int): RList[T] = {
    /*
    [1,2,3].rotate(3) == [1,2,3]
    [1,2,3].rotate(6) == [1,2,3]
    [1,2,3].rotate(4) == [1,2,3].rotate(1)

    [1,2,3].rotate(1) = rotateTailrec([1,2,3], 1, [])
    = rotateTailrec([2,3], 0, [1])
    = [2,3,1]

    [1,2,3].rotate(3) = rotateTailrec([1,2,3], 3, [])
    = rotateTailrec([2,3], 2, [1])
    = rotateTailrec([3], 1, [2,1])
    = rotateTailrec([], 0, [3,2,1])
    = [1,2,3]

    [1,2,3].rotate(4) = rotateTailrec([1,2,3], 4, [])
    = rotateTailrec([2,3], 3, [1])
    = rotateTailrec([3], 2, [2,1])
    = rotateTailrec([], 1, [3,2,1])
    = rotateTailrec([1,2,3], 1, [])
    = [2,3,1]

    Complexity: O(max(N, K))
    */
    @tailrec
    def rotateTailrec(remaining: RList[T], rotationsLeft: Int, buffer: RList[T]): RList[T] = {
      if (remaining.isEmpty && rotationsLeft == 0) this
      else if (remaining.isEmpty) rotateTailrec(this, rotationsLeft, RNil)
      else if (rotationsLeft == 0) remaining ++ buffer.reverse
      else rotateTailrec(remaining.tail, rotationsLeft - 1, remaining.head :: buffer)
    }

    rotateTailrec(this, k, RNil)
  }

  // random sample
  def sample(k: Int): RList[T] = {
    val random = new Random()
    val maxIndex = this.length

    /*
    [1,2,3,4,5].sample(3) = str([], 0)
    = str([2], 1)
    = str([4,2], 2)
    = str([4,4,2], 3)
    = [4,4,2]

    Complexity: O(N * K)
    */
    @tailrec
    def sampleTailrec(accumulator: RList[T], curSamples: Int): RList[T] = {
      if (curSamples == k) accumulator
      else sampleTailrec(this(random.nextInt(maxIndex)) :: accumulator, curSamples + 1)
    }

    /*
    Complexity: O(N * K)
    */
    def sampleElegant = RList.from((1 to k).map(_ => random.nextInt(maxIndex)).map(index => this(index)))

    if (k < 0) RNil
    else sampleElegant
  }

  /**
    * Hard problems
    */
  // sort the list in the order defined by the Ordering object
  def insertionSort[S >: T](ordering: Ordering[S]): RList[S] = {
    /*
    insertSorted(4, [], [1,2,3,5])
    = insertSorted(4, [1], [2,3,5])
    = insertSorted(4, [2,1], [3,5])
    = insertSorted(4, [3,2,1], [5])
    = [3,2,1].reverse + (4 :: [5])
    = [1,2,3,4,5]

    Complexity: O(N)
    */
    @tailrec
    def insertSorted(element: T, before: RList[S], after: RList[S]): RList[S] = {
      if (after.isEmpty || ordering.lteq(element, after.head)) before.reverse ++ (element :: after)
      else insertSorted(element, after.head :: before, after.tail)
    }

    /*
    [3,1,4,2,5].sorted = insertSortTailrec([3,1,4,2,5], [])
    = insertSortTailrec([1,4,2,5], [3])
    = insertSortTailrec([4,2,5], [1,3])
    = insertSortTailrec([2,5], [1,3,4])
    = insertSortTailrec([5], [1,2,3,4])
    = insertSortTailrec([], [1,2,3,4,5])
    = [1,2,3,4,5]

    Complexity: O(N^2)
    */
    @tailrec
    def insertSortTailrec(remaining: RList[T], accumulator: RList[S]): RList[S] = {
      if (remaining.isEmpty) accumulator
      else insertSortTailrec(remaining.tail, insertSorted(remaining.head, RNil, accumulator))
    }

    insertSortTailrec(this, RNil)
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
    val oneToTen = RList.from(1 to 10)

    def testEasyProblems() = {
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

      // test map
      println(aLargeList.map(x => 2 * x))
      // test flatMap
      val time = System.currentTimeMillis()
      aLargeList.flatMap(x => x :: (2 * x) :: RNil) // first flatMap: 700 ms
      println(System.currentTimeMillis() - time)
      // test filter
      println(aLargeList.filter(x => x % 2 == 0))
    }

    def testMediumProblems() = {
      // rle test
      println((1 :: 1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 5 :: 5 :: 5 :: RNil).rle)

      // duplicate test
      println(aSmallList.duplicateEach(4))

      // rotate test
      for {
        i <- 1 to 20
      } println(oneToTen.rotate(i))

      // sample test
      println(aLargeList.sample(10))

      // better flatMap
      println(aSmallList.flatMap(x => x :: (2 * x) :: RNil))
      val time = System.currentTimeMillis()
      aLargeList.flatMap(x => x :: (2 * x) :: RNil) // better flatMap: 7 ms
      println(System.currentTimeMillis() - time)
    }

    def testHardProblems() = {
      val anUnorderedList = 3 :: 1 :: 2 :: 0 :: 8:: 5 :: RNil
      val ordering = Ordering.fromLessThan[Int](_ < _)

      // insertion sort test
      println(anUnorderedList.insertionSort(ordering)) // [0,1,2,3,5,8]
      println(aLargeList.sample(10).insertionSort(ordering))
    }

    testHardProblems()
  }
}