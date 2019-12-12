package com.github.daniel0611.adventofcode2019.week2

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks._

object Day12 extends App {

  type Vector3 = (Int, Int, Int)

  case class Planet(pos: Vector3, vel: Vector3) {

    def gravity(other: Planet): Planet = Planet(pos, (
      vel._1 + direction(pos._1, other.pos._1),
      vel._2 + direction(pos._2, other.pos._2),
      vel._3 + direction(pos._3, other.pos._3)
    ))

    private def direction(pos1: Int, pos2: Int): Int = pos2.compare(pos1)

    def velocity(): Planet = Planet((
      pos._1 + vel._1,
      pos._2 + vel._2,
      pos._3 + vel._3
    ), vel)


    def getEnergy: Int = (pos._1.abs + pos._2.abs + pos._3.abs) *
      (vel._1.abs + vel._2.abs + vel._3.abs)
  }

  def simulate(planets: Array[Planet]): Array[Planet] = simulate(0 to 0, planets)

  def simulate(steps: Range, planets: Array[Planet]) = {
    steps.foldLeft(planets) { (plan, i) =>
      val appliedGravity =
        for (moon1 <- plan) yield {
          var current = moon1
          for {
            moon2 <- plan
            if moon1 != moon2
          } current = current.gravity(moon2)
          current
        }

      val appliedVelocity = appliedGravity.map(_.velocity())
      //      println(i + " " + appliedVelocity.mkString(", "))
      appliedVelocity
    }
  }

  val input = Source.fromInputStream(getClass.getResourceAsStream("/input_day12.txt")).getLines().mkString("\n")

  var planets = input.split("\n").filter(_.nonEmpty).map { line =>
    val nums = line.filter(c => Character.isDigit(c) || c == ',' || c == '-').split(",").map(_.toInt)
    val pos = (nums.head, nums(1), nums(2))
    Planet(pos, (0, 0, 0))
  }


  // Part A
  val finalState = simulate(1 to 1000, planets)
  println {
    finalState.map(_.getEnergy).sum
  }
  println

  // Part B

  @scala.annotation.tailrec
  def gcd(a: Long, b: Long): Long = {
    if (b == 0) a.abs else gcd(b, a % b)
  }

  def lcm(a: Long, b: Long) = (a / gcd(a, b)) * b

  breakable {
    var current = simulate(planets)

    val stepsToReach = ListBuffer(0, 0, 0)

    val initialX = planets.map(_.pos._1)
    val initialY = planets.map(_.pos._2)
    val initialZ = planets.map(_.pos._3)


    var steps = 0
    while (true) {
      current = simulate(current)
      steps += 1

      if (stepsToReach.head == 0 && (current.map(_.pos._1) sameElements initialX) && current.map(_.vel._1).forall(_ == 0)) {
        stepsToReach(0) = steps +1
      }
      if (stepsToReach(1) == 0 && (current.map(_.pos._2) sameElements initialY) && current.map(_.vel._2).forall(_ == 0)) {
        stepsToReach(1) = steps +1
      }
      if (stepsToReach(2) == 0 && (current.map(_.pos._3) sameElements initialZ) && current.map(_.vel._3).forall(_ == 0)) {
        stepsToReach(2) = steps +1
      }

      if (!stepsToReach.contains(0)) {
        println(lcm(lcm(stepsToReach.head.toLong, stepsToReach(1).toLong), stepsToReach(2).toLong))
        break()
      }
    }
  }


}
