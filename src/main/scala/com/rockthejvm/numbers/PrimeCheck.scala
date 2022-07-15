package com.rockthejvm.numbers

import scala.annotation.tailrec

object PrimeCheck {
// Given a number, determine if it is prime
  def isPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeTailrec(currentDivisor: Int): Boolean = {
      if (currentDivisor > Math.sqrt(Math.abs(n))) true
      else n % currentDivisor != 0 && isPrimeTailrec(currentDivisor + 1)
    }

    if (n == 0 || n == 1 || n == -1) false
    else isPrimeTailrec(2)
  }

  def main(args: Array[String]): Unit = {
    println(isPrime(2)) // true
    println(isPrime(15)) // false
    println(isPrime(2003)) // true
    println(isPrime(2731189)) // true
    println(isPrime(-2003)) // true
    println(isPrime(1)) // false
  }
}