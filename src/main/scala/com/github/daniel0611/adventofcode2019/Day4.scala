package com.github.daniel0611.adventofcode2019

object Day4 extends App {

  val range = 245182 to 790572
  val allowedRange = 100000 to 999999

  // Method for Part A
  def isValid1(num: Int): Boolean = {
    val list = num.toString.map(_.toInt).toList

    allowedRange.contains(num) &&
      groupConsecutive(list).exists(_.size > 1) &&
      list == list.sorted
  }

  // Method for Part B
  def isValid2(num: Int): Boolean = {
    val list = num.toString.map(_.toInt).toList

    allowedRange.contains(num) &&
      groupConsecutive(list).exists(_.size == 2) &&
      list == list.sorted
  }

  def groupConsecutive[T](list: List[T]): List[List[T]] = list match {
    case head :: tail =>
      val (t1, t2) = tail.span(_ == head)
      (head :: t1) :: groupConsecutive(t2)
    case _ => Nil
  }

  // Part A
  println(range.count(isValid1))

  println()

  // Part B
  println(range.count(isValid2))
}
