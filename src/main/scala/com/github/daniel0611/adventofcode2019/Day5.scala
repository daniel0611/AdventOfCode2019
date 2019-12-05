package com.github.daniel0611.adventofcode2019

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.{Source, StdIn}

object Day5 extends App {

  val code = Source.fromInputStream(getClass.getResourceAsStream("/input_day5.txt"))
    .getLines().mkString.split(",").map(_.toInt)

  type RAM = mutable.Seq[Int]

  @tailrec
  def runOpCode(ram: RAM, i: Int = 0): Unit = ram.drop(i) match {
    case opCode +: (inPos1: Int) +: (inPos2: Int) +: (outPos: Int) +: _ if opCode % 100 == 1 =>
      ram(outPos) = getValue(ram, opCode, 1, inPos1) + getValue(ram, opCode, 2, inPos2)
      runOpCode(ram, i + 4)

    case opCode +: (inPos1: Int) +: (inPos2: Int) +: (outPos: Int) +: _ if opCode % 100 == 2 =>
      ram(outPos) = getValue(ram, opCode, 1, inPos1) * getValue(ram, opCode, 2, inPos2)
      runOpCode(ram, i + 4)

    case opCode +: (outPos: Int) +: _ if opCode % 100 == 3 =>
      print("Please enter a input: ")
      ram(outPos) = StdIn.readInt()
      runOpCode(ram, i + 2)

    case opCode +: (inPos: Int) +: _ if opCode % 100 == 4 =>
      println(ram(inPos))
      runOpCode(ram, i + 2)

    case opCode +: (inPos: Int) +: (outPos: Int) +: _ if opCode % 100 == 5 =>
      if(getValue(ram, opCode, 1, inPos) != 0)
        runOpCode(ram, getValue(ram, opCode, 2, outPos))
      else
        runOpCode(ram, i + 3)

    case opCode +: (inPos: Int) +: (outPos: Int) +: _ if opCode % 100 == 6 =>
      if(getValue(ram, opCode, 1, inPos) == 0)
        runOpCode(ram, getValue(ram, opCode, 2, outPos))
      else
        runOpCode(ram, i + 3)

    case opCode +: (inPos1: Int) +: (inPos2: Int) +: (outPos: Int) +: _ if opCode % 100 == 7 =>
      val value1 = getValue(ram, opCode, 1, inPos1)
      val value2 = getValue(ram, opCode, 2, inPos2)

      ram(outPos) = if (value1 < value2) 1
      else 0

      runOpCode(ram, i + 4)

    case opCode +: (inPos1: Int) +: (inPos2: Int) +: (outPos: Int) +: _ if opCode % 100 == 8 =>
      val value1 = getValue(ram, opCode, 1, inPos1)
      val value2 = getValue(ram, opCode, 2, inPos2)

      ram(outPos) = if (value1 == value2) 1
      else 0

      runOpCode(ram, i + 4)

    case 99 +: _ => // finished
    case opCode +: _ => println(s"ERROR unknown opCode: $opCode")
  }

  def getValue(ram: RAM, opCode: Int, paramPos: Int, param: Int) = {
    val parameterMode = opCode.toString.toCharArray.reverse.drop(1).map(x => s"$x".toInt).applyOrElse(paramPos, (_: Any) => 0) // FIXME

    parameterMode match {
      case 1 => param
      case 0 => ram(param.abs)

      case _ => throw new Exception(opCode.toString + " " + parameterMode)
    }
  }

  val ram: RAM = code
  runOpCode(ram)
}
