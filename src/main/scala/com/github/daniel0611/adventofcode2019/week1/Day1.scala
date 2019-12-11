package com.github.daniel0611.adventofcode2019.week1

import scala.io.Source

object Day1 extends App {
  private val data = Source.fromInputStream(getClass.getResourceAsStream("/input_day1.txt"))
    .getLines().map(_.toInt).toList

  // Part A
  def getFuel(mass: Int) = (mass / 3) - 2

  println {
    data.map(getFuel).sum
  }

  // Part B
  def getActualFuel(mass: Int): Int = {
    val fuel = getFuel(mass)
    if (fuel > 0)
      fuel + getActualFuel(fuel)
    else
      0
  }

  println {
    data.map(getActualFuel).sum
  }
}
