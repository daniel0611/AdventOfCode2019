package com.github.daniel0611.adventofcode2019.week1

import scala.collection.mutable
import scala.io.Source
import scala.annotation.tailrec

object Day7 extends App {
  private val code = Source.fromInputStream(getClass.getResourceAsStream("/input_day7.txt"))
    .getLines().mkString.split(",").map(_.toInt)

  class Amplifier(phaseSetting: Int) {

    type RAM = mutable.Seq[Int]
    type ResumePosition = (Int, Int)

    var output: Option[Int] = None
    val ram: RAM = code.clone
    private var resumePos: Option[ResumePosition] = None

    runOpCode(ram) // Start executing
    resumeExecution(phaseSetting)

    def resumeExecution(input: Int): Boolean = {
      if (resumePos.isEmpty) throw new Exception("Execution hasn't been stopped!")
      else {
        ram(resumePos.get._2) = input
        val i = resumePos.get._1
        resumePos = None
        runOpCode(ram, i)
      }
    }

    // OpCode execution

    @tailrec
    private def runOpCode(ram: RAM, i: Int = 0): Boolean = {
      val newPos = ram.drop(i) match {
        case opCode +: (inPos1: Int) +: (inPos2: Int) +: (outPos: Int) +: _ if opCode % 100 == 1 =>
          ram(outPos) = getValue(ram, opCode, 1, inPos1) + getValue(ram, opCode, 2, inPos2)
          i + 4

        case opCode +: (inPos1: Int) +: (inPos2: Int) +: (outPos: Int) +: _ if opCode % 100 == 2 =>
          ram(outPos) = getValue(ram, opCode, 1, inPos1) * getValue(ram, opCode, 2, inPos2)
          i + 4

        case opCode +: (outPos: Int) +: _ if opCode % 100 == 3 =>
          resumePos = Some((i + 2, outPos))
          return false

        case opCode +: (inPos: Int) +: _ if opCode % 100 == 4 =>
          output = Some(ram(inPos))
          i + 2

        case opCode +: (inPos: Int) +: (outPos: Int) +: _ if opCode % 100 == 5 =>
          if (getValue(ram, opCode, 1, inPos) != 0)
            getValue(ram, opCode, 2, outPos)
          else
            i + 3

        case opCode +: (inPos: Int) +: (outPos: Int) +: _ if opCode % 100 == 6 =>
          if (getValue(ram, opCode, 1, inPos) == 0)
            getValue(ram, opCode, 2, outPos)
          else
            i + 3

        case opCode +: (inPos1: Int) +: (inPos2: Int) +: (outPos: Int) +: _ if opCode % 100 == 7 =>
          val value1 = getValue(ram, opCode, 1, inPos1)
          val value2 = getValue(ram, opCode, 2, inPos2)

          ram(outPos) = if (value1 < value2) 1
          else 0

          i + 4

        case opCode +: (inPos1: Int) +: (inPos2: Int) +: (outPos: Int) +: _ if opCode % 100 == 8 =>
          val value1 = getValue(ram, opCode, 1, inPos1)
          val value2 = getValue(ram, opCode, 2, inPos2)

          ram(outPos) = if (value1 == value2) 1
          else 0

          i + 4

        case 99 +: _ => return true // finished
        case opCode +: _ => throw new Exception(s"ERROR unknown opCode: $opCode");
      }
      runOpCode(ram, newPos)
    }

    private def getValue(ram: RAM, opCode: Int, paramPos: Int, param: Int) = {
      val parameterMode = opCode.toString.toCharArray.reverse.drop(1).map(x => s"$x".toInt).applyOrElse(paramPos, (_: Any) => 0)

      parameterMode match {
        case 1 => param
        case 0 => ram(param.abs)

        case _ => throw new Exception(opCode.toString + " " + parameterMode)
      }
    }
  }

  // Part A

  println {
    (1234 to 43210).filter(num => f"$num%05d".toCharArray.distinct.length == 5 && num.toString.split("").forall(_.toInt < 5))
      .map { comb =>
        val res = f"$comb%05d".split("").map(_.toInt).toList.fold(0)((input, phase) => {
          val amp = new Amplifier(phase)
          amp.resumeExecution(input)
          amp.output.get
        })
        println(f"$res ($comb%05d)")
        res
      }.max
  }

  println()

  // Part B

  println {
    (56789 to 98765).filter(num => f"$num%05d".toCharArray.distinct.length == 5 && num.toString.split("").forall(_.toInt >= 5))
      .map { comb =>
        val amps = f"$comb%05d".split("").map(_.toInt).toList.map(e => new Amplifier(e))

        var finished = 0

        while (finished < 5) {
          finished = 0

          for (i <- amps.indices) {
            val previousAmp = if (i == 0) amps.last else amps(i - 1)
            val input = if (previousAmp.output.isEmpty) 0 else previousAmp.output.get

            if (amps(i).resumeExecution(input))
              finished += 1
          }
        }

        println(f"${amps.last.output.get} ($comb%05d)")
        amps.last.output.get
      }.max
  }
}
