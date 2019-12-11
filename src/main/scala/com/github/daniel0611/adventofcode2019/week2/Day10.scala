package com.github.daniel0611.adventofcode2019.week2

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {

  private val input = Source.fromInputStream(getClass.getResourceAsStream("/input_day10.txt")).getLines.mkString("\n")


  private def getReachable(from: Asteroid, asteroids: Seq[Asteroid]): Seq[Asteroid] = {
    asteroids
      .filter(_ != from)
      .sortBy(-from.distanceSquared(_))
      .foldLeft(List[Asteroid]())((visible, a) =>
        if (visible.isEmpty || !visible.exists(b => from.inLine(a, b)))
          visible ++ Seq(a)
        else visible
      )
  }

  private def parseInput(in: String): Seq[Asteroid] = {
    val inMatrix = in.split("\n").map(_.toCharArray)

    for {
      x <- inMatrix.maxBy(_.length).indices
      y <- inMatrix.indices
      if inMatrix(y)(x) == '#'
    } yield Asteroid(x, y)
  }

  case class Asteroid(x: Int, y: Int) {

    def distanceSquared(b: Asteroid): Int = {
      val (dx, dy) = (math.abs(x - b.x), math.abs(y - b.y))
      dx * dx + dy * dy
    }

    def inLine(b: Asteroid, c: Asteroid): Boolean = {
      if ((b.x > x) == (c.x > x) && (b.y > y) == (c.y > y)) {
        tanOfAngle(b) == tanOfAngle(c)
      } else {
        false
      }
    }

    def tanOfAngle(b: Asteroid): Float = {
      val (dx, dy) = (math.abs(x - b.x), math.abs(y - b.y))
      dy.toFloat / dx.toFloat
    }

    def angle(b: Asteroid): Float = {
      if (b.x > x && b.y >= y) {
        (math.Pi * 0.5 - math.atan(tanOfAngle(b))).toFloat
      } else if (b.x < x && b.y >= y) {
        (math.Pi * 1.5 + math.atan(tanOfAngle(b))).toFloat
      } else if (b.x < x && b.y < y) {
        (math.Pi * 1.5 - math.atan(tanOfAngle(b))).toFloat
      } else if (b.x > x && b.y < y) {
        (math.Pi * 0.5 + math.atan(tanOfAngle(b))).toFloat
      } else if (b.y >= y) {
        0
      } else {
        math.Pi.toFloat
      }
    }
  }

  // Part A

  val asteroids = parseInput(input)

  val (station, reachable) = asteroids.map(a => (a, getReachable(a, asteroids).size)).maxBy(_._2)

  println(reachable)
  println(station)
  println()

  // Part B

  @tailrec
  def destroyAsteroids(s: Asteroid, destroyed: Seq[Asteroid] = Seq()): Seq[Asteroid] = {
    val visible = getReachable(s, asteroids.filter(!destroyed.contains(_))).sortBy(s.angle).reverse

    if (visible.isEmpty)
      destroyed
    else {
      destroyAsteroids(s, destroyed ++ visible)
    }
  }

  println {
    val as = destroyAsteroids(station, asteroids)(199)
    as.x * 100 + as.y
  }
}
