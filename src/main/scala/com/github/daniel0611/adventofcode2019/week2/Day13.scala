package com.github.daniel0611.adventofcode2019.week2

import com.github.daniel0611.adventofcode2019.IntCodeComputer

import scala.collection.mutable.{Map => MMap}
import scala.io.Source

object Day13 extends App {

  private val code = Source.fromInputStream(getClass.getResourceAsStream("/input_day13.txt"))
    .getLines().mkString.split(",").map(_.toLong)

  // Part A
  {
    val tiles: MMap[(Int, Int), Int] = MMap()
    val computer = new IntCodeComputer(code, false, false)
    computer.compute()

    computer.output.dequeueAll(_ => true)
      .grouped(3)
      .map(out => (out.head.toInt, out(1).toInt) -> out(2).toInt)
      .foreach(tiles += _)

    println {
      tiles.count(_._2 == 2)
    }
  }

  println

  // Part B
  {
    var score = 0
    val tiles: MMap[(Int, Int), Int] = MMap()
    val computer = new IntCodeComputer(code, false, true)
    computer.alterRAMValue(0, 2)

    var ballX, paddleX = 0

    while (!computer.finished) {
      val outputValues = for (_ <- 0 to 2) yield {
        computer.compute()

        if (!computer.finished) computer.output.dequeue()
        else 0
      }

      if (outputValues.head == -1 && outputValues(1) == 0) {
        score = outputValues(2).toInt
      } else {
        val pos = (outputValues.head.toInt, outputValues(1).toInt)
        val t = outputValues(2)
        tiles.remove(pos)
        tiles(pos) = t.toInt

        if (t == 4) {
          ballX = pos._1
        }
        if (t == 3) {
          paddleX = pos._1
        }
      }

      computer.input.dequeueAll(_ => true)
      computer.input.enqueue(getJoystickPosition(ballX, paddleX))
    }

    def getJoystickPosition(ballX: Int, paddleX: Int) = {
      if (paddleX > ballX) -1
      else if (paddleX < ballX) 1
      else 0
    }

    println(s"Blocks left: ${tiles.count(_._2 == 2)}")
    println(s"Score: $score")
  }

}
