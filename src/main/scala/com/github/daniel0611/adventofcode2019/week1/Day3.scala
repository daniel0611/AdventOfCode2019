package com.github.daniel0611.adventofcode2019.week1

import scala.io.Source

object Day3 extends App {

  type WirePoint = (Int, Int)
  type WirePath = List[WirePoint]

  def renderWire(text: String): WirePath = {
    text.split(",")
      .foldLeft[WirePath](List()) {
        case (path, command) =>
          val action = command.head
          val rest = command.drop(1)

          val count = rest.toInt
          val (currentX, currentY) = path.lastOption.getOrElse(0, 0)
          val movement = getMovement(action, count)
          val newPath = for (deltaX <- movement._1; deltaY <- movement._2) yield (currentX + deltaX, currentY + deltaY)
          path ++ newPath
      }
  }

  def getMovement(c: Char, count: Int) = c match {
    case 'U' => (0 to 0, 1 to count)
    case 'D' => (0 to 0, -1 to -count by -1)
    case 'R' => (1 to count, 0 to 0)
    case 'L' => (-1 to -count by -1, 0 to 0)
    case _ =>       (0 to 0, 0 to 0)
  }

  def getDistanceToNearestCrossing(w1: WirePath, w2: WirePath): Option[Int] = {
    w1.intersect(w2)
      .filter(_ != (0, 0))
      .map(point => point._1.abs + point._2.abs)
      .sorted // Sort ascending
      .headOption
  }

  def getSteps(w1: WirePath, w2: WirePath): Option[Int] = {
    w1.intersect(w2)
      .filter(_ != (0, 0))
      .map(point => (w1.indexOf(point) + 1, w2.indexOf(point) + 1))
      .map(steps => steps._1 + steps._2)
      .sorted
      .headOption
  }

  private val data = Source.fromInputStream(getClass.getResourceAsStream("/input_day3.txt")).getLines()

  val s1 = data.next()
  val s2 = data.next()

  val wire1 = renderWire(s1)
  val wire2 = renderWire(s2)

  // Part A
  println(getDistanceToNearestCrossing(wire1, wire2))

  println()

  /// Part B
  println(getSteps(wire1, wire2))
}
