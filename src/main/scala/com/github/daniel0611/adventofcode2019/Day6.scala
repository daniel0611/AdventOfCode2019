package com.github.daniel0611.adventofcode2019

import scala.io.Source

object Day6 extends App {

  private val input = Source.fromInputStream(getClass.getResourceAsStream("/input_day6.txt")).getLines().mkString("\n")

  // Map of planet -> parent
  val planets: Map[String, String] = input.split("\n")
    .map(_.split("\\)"))
    .filter(_.length >= 2)
    .map(e => (e(1).toUpperCase, e.head.toUpperCase))
    .toMap

  def getAllOrbits(name: String): List[String] = {
    Iterator.iterate(name)(n => planets.get(n).orNull).takeWhile(_ != null).filter(_ != "COM").toList
  }

  // Part A
  println {
    planets.keys.toList.map(p => getAllOrbits(p).size).sum
  }

  // Part B
  println {
    // Get parent planets of persons
    val youOrbit = getAllOrbits(planets("YOU"))
    val sanOrbit = getAllOrbits(planets("SAN"))

    val nearestIntersect = youOrbit.intersect(sanOrbit).head

    val distance = youOrbit.indexOf(nearestIntersect) + sanOrbit.indexOf(nearestIntersect)

    distance
  }
}
