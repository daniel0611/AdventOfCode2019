package com.github.daniel0611.adventofcode2019

import scala.io.Source

object Day8 extends App {
  private val input = Source.fromInputStream(getClass.getResourceAsStream("/input_day8.txt")).getLines().mkString
  private val width = 25
  private val height = 6

  type Pixel = Int
  type Row = List[Pixel]
  type Layer = List[Row]
  type Image = List[Layer]

  // Part A

  def decodeImage(input: String): Image = {
    val nums = input.split("").map(_.toInt).toList

    val rows: List[Row] = divideList(nums, width)

    divideList(rows, height)
  }

  private def divideList[T](input: List[T], length: Int): List[List[T]] = {
    val (part, tail) = input.splitAt(length)

    if (tail.size >= length)
      part +: divideList(tail, length)
    else
      part :: Nil
  }


  println {
    val selectedLayer = decodeImage(input)
      .minBy(x => x.flatten.count(x => x == 0)).flatten

    selectedLayer.count(_ == 1) *
      selectedLayer.count(_ == 2)
  }


  // Part B

  def renderPixel(upper: Pixel, lower: Pixel): Pixel = upper match {
    case 2 => lower // transparent
    case _ => upper
  }

  def renderImage(image: Image): Layer = {
    val initialLayer = image.head
    image.drop(1).foldLeft(initialLayer)((previous, current) => {
      previous.zip(current)
        .map(rows => {
          rows._1.zip(rows._2).map(pixels => renderPixel(pixels._1, pixels._2))
        })
    })
  }

  println {
    val layers = decodeImage(input)
    val finalImage = renderImage(layers)

    finalImage.map(_.map {
      case 1 => "*"
      case _ => " "
    }.mkString).mkString("\n")
  }
}
