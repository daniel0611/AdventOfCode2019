package com.github.daniel0611.adventofcode2019.week1

import scala.collection.mutable
import scala.io.Source

object Day2 extends App {

  private val code = Source.fromInputStream(getClass.getResourceAsStream("/input_day2.txt"))
    .getLines.mkString.split(",").map(_.toInt)

  @scala.annotation.tailrec
  def runIntCode(code: mutable.Seq[Int], i: Int = 0): Unit = code.drop(i) match {
    case 1 +: (in1: Int) +: (in2: Int) +: (out: Int) +: _ =>
      code(out) = code(in1) + code(in2)
      runIntCode(code, i + 4)

    case 2 +: (in1: Int) +: (in2: Int) +: (out: Int) +: _ =>
      code(out) = code(in1) * code(in2)
      runIntCode(code, i + 4)

    case 99 +: _ => // finished
    case _ => println("ERROR")
  }

  def part1(): Unit = {
    val ram = code.clone()
    ram(1) = 12
    ram(2) = 2
    runIntCode(ram)
    println(ram.head)
  }

  def part2(): Unit = {
    for (noun <- 0 to 99; verb <- 0 to 99) {
      val ram = code.clone()
      ram(1) = noun
      ram(2) = verb
      runIntCode(ram)
      if (ram.head == 19690720) {
        println(noun)
        println(verb)
      }
    }
  }

  part1()
  println()
  part2()
}
