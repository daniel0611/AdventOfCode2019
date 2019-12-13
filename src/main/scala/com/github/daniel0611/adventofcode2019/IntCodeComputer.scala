package com.github.daniel0611.adventofcode2019

import com.github.daniel0611.adventofcode2019.IntCodeComputer.RAM

import scala.annotation.tailrec
import scala.collection.mutable


object IntCodeComputer {
  type RAM = mutable.Map[Int, Long]
}

class IntCodeComputer(code: Array[Long], pauseOnInput: Boolean, pauseOnOutput: Boolean) {

  var pointer = 0
  var relativeBase = 0
  var finished = false
  val input: mutable.Queue[Long] = mutable.Queue[Long]()
  var output: mutable.Queue[Long] = mutable.Queue[Long]()

  private val codeMap = code.zipWithIndex.map(x => x._2 -> x._1)
  private val ram: RAM = mutable.Map(codeMap: _*)

  def alterRAMValue(index: Int, value: Long): Unit = {
    ram(index) = value
  }

  @tailrec
  final def compute(): Unit = {
    val pointerRange = pointer to pointer + 3
    ram.toList.filter(x => pointerRange.contains(x._1)).sortBy(_._1).map(_._2) match {
      case (opCode: Long) +: (inPos1: Long) +: (inPos2: Long) +: (outPos: Long) +: _ if opCode % 100 == 1 =>
        val result = getValue(opCode, 1, inPos1) + getValue(opCode, 2, inPos2)
        setValue(opCode, 3, outPos, result)
        pointer += 4

      case (opCode: Long) +: (inPos1: Long) +: (inPos2: Long) +: (outPos: Long) +: _ if opCode % 100 == 2 =>
        val result = getValue(opCode, 1, inPos1) * getValue(opCode, 2, inPos2)
        setValue(opCode, 3, outPos, result)
        pointer += 4

      case (opCode: Long) +: (outPos: Long) +: _ if opCode % 100 == 3 =>
        if (input.nonEmpty) {
          setValue(opCode, 1, outPos, input.dequeue())
        } else {
          return // pause execution
        }
        pointer += 2

        if (pauseOnInput)
          return

      case (opCode: Long) +: (inPos: Long) +: _ if opCode % 100 == 4 =>
        val value = getValue(opCode, 1, inPos)
        output.enqueue(value)
        pointer += 2
        if (pauseOnOutput)
          return

      case (opCode: Long) +: (inPos: Long) +: (outPos: Long) +: _ if opCode % 100 == 5 =>
        pointer = if (getValue(opCode, 1, inPos) != 0)
          getValue(opCode, 2, outPos).toInt
        else
          pointer + 3

      case (opCode: Long) +: (inPos: Long) +: (outPos: Long) +: _ if opCode % 100 == 6 =>
        pointer = if (getValue(opCode, 1, inPos) == 0)
          getValue(opCode, 2, outPos).toInt
        else
          pointer + 3

      case (opCode: Long) +: (inPos1: Long) +: (inPos2: Long) +: (outPos: Long) +: _ if opCode % 100 == 7 =>
        val value1 = getValue(opCode, 1, inPos1)
        val value2 = getValue(opCode, 2, inPos2)

        setValue(opCode, 3, outPos,
          if (value1 < value2) 1
          else 0
        )

        pointer += 4

      case (opCode: Long) +: (inPos1: Long) +: (inPos2: Long) +: (outPos: Long) +: _ if opCode % 100 == 8 =>
        val value1 = getValue(opCode, 1, inPos1)
        val value2 = getValue(opCode, 2, inPos2)

        setValue(
          opCode, 3, outPos, if (value1 == value2) 1
          else 0
        )

        pointer += 4

      case (opCode: Long) +: (in: Long) +: _ if opCode % 100 == 9 =>
        relativeBase += getValue(opCode, 1, in).toInt
        pointer += 2

      case 99 +: _ => // finished
        finished = true
        return
      case opCode +: _ => println(s"ERROR unknown opCode: $opCode")
    }

    compute()
  }

  private def getValue(opCode: Long, paramPos: Int, param: Long): Long = {
    getParameterMode(opCode, paramPos) match {
      case 0 => ram.applyOrElse(param.toInt.abs, (_: Any) => 0)
      case 1 => param
      case 2 => ram.applyOrElse(param.toInt + relativeBase, (_: Any) => 0)

      case x => throw new Exception(opCode.toString + " " + x)
    }
  }

  private def setValue(opCode: Long, paramPos: Int, outParam: Long, value: Long): Unit = {
    getParameterMode(opCode, paramPos) match {
      case 2 => ram(outParam.toInt + relativeBase) = value
      case 0 => ram(outParam.toInt) = value
    }
  }

  private def getParameterMode(opCode: Long, paramPos: Int): Int =
    opCode.toString.split("").reverse.drop(1).map(x => x.toInt).applyOrElse(paramPos, (_: Any) => 0)
}
