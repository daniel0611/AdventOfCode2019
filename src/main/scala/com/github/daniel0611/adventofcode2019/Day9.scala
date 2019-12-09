package com.github.daniel0611.adventofcode2019

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.{Source, StdIn}

object Day9 extends App {

  type RAM = mutable.Seq[Long]

  private implicit def long2int(l: Long): Int = l.toInt

  private val code: RAM = Source.fromInputStream(getClass.getResourceAsStream("/input_day9.txt")).getLines().mkString
    .split(",").map(_.toLong)

  @tailrec
  def runOpCode(i: Int = 0)(implicit ram: RAM, relativeBase: Int = 0): Unit = ram.slice(i, i + 4) match {
    case opCode +: (inPos1: Long) +: (inPos2: Long) +: (outPos: Long) +: _ if opCode % 100 == 1 =>
      val result = getValue(opCode, 1, inPos1) + getValue(opCode, 2, inPos2)
      setValue(opCode, 3, outPos, result)
      runOpCode(i + 4)

    case opCode +: (inPos1: Long) +: (inPos2: Long) +: (outPos: Long) +: _ if opCode % 100 == 2 =>
      val result = getValue(opCode, 1, inPos1) * getValue(opCode, 2, inPos2)
      setValue(opCode, 3, outPos, result)
      runOpCode(i + 4)

    case opCode +: (outPos: Long) +: _ if opCode % 100 == 3 =>
      print("Please enter a input: ")
      setValue(opCode, 1, outPos, StdIn.readLong())
      runOpCode(i + 2)

    case opCode +: (inPos: Long) +: _ if opCode % 100 == 4 =>
      println(getValue(opCode, 1, inPos))
      runOpCode(i + 2)

    case opCode +: (inPos: Long) +: (outPos: Long) +: _ if opCode % 100 == 5 =>
      if (getValue(opCode, 1, inPos) != 0)
        runOpCode(getValue(opCode, 2, outPos))
      else
        runOpCode(i + 3)

    case opCode +: (inPos: Long) +: (outPos: Long) +: _ if opCode % 100 == 6 =>
      if (getValue(opCode, 1, inPos) == 0)
        runOpCode(getValue(opCode, 2, outPos))
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
      runOpCode(i + 2)(ram, relativeBase + newRelativeBase)

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
      case 2 => ram(outParam + relativeBase) = value
      case 0 => ram(outParam) = value
    }
  }

  private def getParameterMode(opCode: Long, paramPos: Int): Int =
    opCode.toString.split("").reverse.drop(1).map(x => x.toInt).applyOrElse(paramPos, (_: Any) => 0)

  private val ram = new Array[Long](10000) // Pretty ugly, but it works
  for ((value, index) <- code.zipWithIndex) {
    ram(index) = value
  }
  runOpCode()(ram)
}
