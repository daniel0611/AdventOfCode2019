package com.github.daniel0611.adventofcode2019.week2


import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day11 extends App {
  type RAM = mutable.Seq[Long]

  class Robot(code: RAM, startOnWhite: Boolean) {
    type Position = (Int, Int)

    val data: mutable.Map[Position, Boolean] = mutable.Map()
    data += (0, 0) -> startOnWhite

    val paintedPositions: mutable.Set[Position] = mutable.Set()

    private var currentPos: Position = (0, 0)
    private var direction = 0
    private var secondOut = false
    private implicit val ram: RAM = new Array[Long](10000).zipWithIndex.map(x => code.applyOrElse(x._2, (_: Any) => 0L))

    def run(): Unit = runOpCode()

    @tailrec
    private def runOpCode(i: Int = 0)(implicit ram: RAM, relativeBase: Int = 0): Unit = ram.slice(i, i + 4) match {
      case opCode +: (inPos1: Long) +: (inPos2: Long) +: (outPos: Long) +: _ if opCode % 100 == 1 =>
        val result = getValue(opCode, 1, inPos1) + getValue(opCode, 2, inPos2)
        setValue(opCode, 3, outPos, result)
        runOpCode(i + 4)

      case opCode +: (inPos1: Long) +: (inPos2: Long) +: (outPos: Long) +: _ if opCode % 100 == 2 =>
        val result = getValue(opCode, 1, inPos1) * getValue(opCode, 2, inPos2)
        setValue(opCode, 3, outPos, result)
        runOpCode(i + 4)

      case opCode +: (outPos: Long) +: _ if opCode % 100 == 3 =>
        val value = if (data.getOrElse(currentPos, false)) 1 else 0
        setValue(opCode, 1, outPos, value)
        runOpCode(i + 2)

      case opCode +: (inPos: Long) +: _ if opCode % 100 == 4 =>
        val value = getValue(opCode, 1, inPos)
        if (!secondOut) {
          data(currentPos) = if (value == 1) true else false
          paintedPositions += currentPos
        } else {
          direction = if (value == 1) (direction + 1) % 4
          else {
            if (direction == 0) 3
            else direction - 1
          }

          currentPos = direction match {
            case 0 => (currentPos._1, currentPos._2 + 1)
            case 1 => (currentPos._1 + 1, currentPos._2)
            case 2 => (currentPos._1, currentPos._2 - 1)
            case 3 => (currentPos._1 - 1, currentPos._2)
            case _ => currentPos
          }
        }

        secondOut = !secondOut
        runOpCode(i + 2)

      case opCode +: (inPos: Long) +: (outPos: Long) +: _ if opCode % 100 == 5 =>
        if (getValue(opCode, 1, inPos) != 0)
          runOpCode(getValue(opCode, 2, outPos).toInt)
        else
          runOpCode(i + 3)

      case opCode +: (inPos: Long) +: (outPos: Long) +: _ if opCode % 100 == 6 =>
        if (getValue(opCode, 1, inPos) == 0)
          runOpCode(getValue(opCode, 2, outPos).toInt)
        else
          runOpCode(i + 3)

      case opCode +: (inPos1: Long) +: (inPos2: Long) +: (outPos: Long) +: _ if opCode % 100 == 7 =>
        val value1 = getValue(opCode, 1, inPos1)
        val value2 = getValue(opCode, 2, inPos2)

        setValue(opCode, 3, outPos,
          if (value1 < value2) 1
          else 0
        )

        runOpCode(i + 4)

      case opCode +: (inPos1: Long) +: (inPos2: Long) +: (outPos: Long) +: _ if opCode % 100 == 8 =>
        val value1 = getValue(opCode, 1, inPos1)
        val value2 = getValue(opCode, 2, inPos2)

        setValue(
          opCode, 3, outPos, if (value1 == value2) 1
          else 0
        )

        runOpCode(i + 4)

      case opCode +: (in: Long) +: _ if opCode % 100 == 9 =>
        val newRelativeBase = getValue(opCode, 1, in)
        runOpCode(i + 2)(ram, relativeBase + newRelativeBase.toInt)

      case 99 +: _ => // finished
      case opCode +: _ => println(s"ERROR unknown opCode: $opCode")
    }

    private def getValue(opCode: Long, paramPos: Int, param: Long)(implicit ram: RAM, relativeBase: Int): Long = {
      getParameterMode(opCode, paramPos) match {
        case 0 => ram.applyOrElse(param.toInt.abs, (_: Any) => 0)
        case 1 => param
        case 2 => ram.applyOrElse(param.toInt + relativeBase, (_: Any) => 0)

        case x => throw new Exception(opCode.toString + " " + x)
      }
    }

    private def setValue(opCode: Long, paramPos: Int, outParam: Long, value: Long)(implicit ram: RAM, relativeBase: Int): Unit = {
      getParameterMode(opCode, paramPos) match {
        case 2 => ram(outParam.toInt + relativeBase) = value
        case 0 => ram(outParam.toInt) = value
      }
    }

    private def getParameterMode(opCode: Long, paramPos: Int): Int =
      opCode.toString.split("").reverse.drop(1).map(x => x.toInt).applyOrElse(paramPos, (_: Any) => 0)

  }


  private val code = Source.fromInputStream(getClass.getResourceAsStream("/input_day11.txt"))
    .getLines().mkString.split(",").map(_.toLong)

  // Part A

  println {
    val robot = new Robot(code, false)
    robot.run()

    robot.paintedPositions.size
  }

  // Part B
  val robot = new Robot(code, true)
  robot.run()
  val d = robot.data.filter(_._2)

  val xRange = 0 to 40
  val yRange = 0 to -5 by -1

  for (y <- yRange) {
    for (x <- xRange) {
      if (d.contains((x,y)))
        print("*")
      else print(" ")
    }
    println()
  }

}
