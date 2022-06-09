package com.example.monobankapiwrapper

import scala.annotation.tailrec

object NumberConverter {

  val EMPTY = List[String]()

  val hexToBinValues = Map(
    "0" -> "0000", "1" -> "0001", "2" -> "0010", "3" -> "0011",
    "4" -> "0100", "5" -> "0101", "6" -> "0110", "7" -> "0111",
    "8" -> "1000", "9" -> "1001", "A" -> "1010", "B" -> "1011",
    "C" -> "1100", "D" -> "1101", "E" -> "1110", "F" -> "1111"
  )

  val revHexToBin: Map[String, String] = hexToBinValues map (_.swap)

  val hexToDecValues = Map(
    "0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3,
    "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7,
    "8" -> 8, "9" -> 9, "A" -> 10, "B" -> 11,
    "C" -> 12, "D" -> 13, "E" -> 14, "F" -> 15
  )

  val revHexToDec: Map[Int, String] = hexToDecValues map (_.swap)

  def powersOfX(x: Int, start: Int = 1): LazyList[Int] = LazyList.cons(start, powersOfX(x, start * x))

  def powersOf2UpToN(n: Int) = powersOfX(2) takeWhile (_ <= n)

  def powersOf16UpToN(n: Int) = powersOfX(16) takeWhile (_ <= n)

  def nthPowersOf2(n: Int) = powersOfX(2) take n

  def nthPowersOf16(n: Int) = powersOfX(16) take n

  def superSplit(s: String): List[String] = s.split("").filter(_ != "").toList

  def extractStr(x: Option[String]): String = x match {
    case Some(str) => str;
    case _ => "0"
  }

  def extractInt(x: Option[Int]): Int = x match {
    case Some(i) => i;
    case _ => 1
  }

  def  decimalToBinary(decimal: String): String = {
    @tailrec
    def calculate(num: Int, powers: LazyList[Int], accum: String): String = {
      if (powers.isEmpty) accum
      else if (powers.head <= num) calculate(num - powers.head, powers.tail, accum + "1")
      else calculate(num, powers.tail, accum + "0")
    }

    val posDec = decimal.toInt.abs
    val pwrs = powersOf2UpToN(posDec).reverse
    val answer = calculate(posDec, pwrs, "")

    answer
  }

  def binaryToDecimal(binary: String): Int = {
    @tailrec
    def calculate(num: String, powers: LazyList[Int], accum: Int): String = {
      if (powers.isEmpty) accum.toString
      else calculate(num.substring(1),
        powers.tail,
        accum + (if (num.charAt(0) == '1') powers.head else 0)
      )
    }

    val pwrs = nthPowersOf2(binary.length).reverse
    val answer = calculate(binary, pwrs, 0)

    answer.toInt
  }

  def decimalToHex(decimal: String): String = {
    @tailrec
    def calculate(num: Int, powers: LazyList[Int], accum: String): String = {
      if (powers.isEmpty) accum
      else if (powers.head <= num) {
        val dividesBy = num / powers.head
        val hexSymbol = extractStr(revHexToDec.get(dividesBy))
        calculate(num - (powers.head * dividesBy), powers.tail, accum + hexSymbol)
      } else calculate(num, powers.tail, accum + "0")
    }

    val pwrs = powersOf16UpToN(decimal.toInt).reverse
    val answer = calculate(decimal.toInt, pwrs, "")

    answer
  }

  def hexToDecimal(hex: String): Int = {
    val pwrs = nthPowersOf16(hex.length).reverse
//    val zippedList = (pwrs, superSplit(hex)).zipped.toList
    val zippedList = pwrs zip superSplit(hex)
    val extrList = zippedList map (x => (x._1, extractInt(hexToDecValues.get(x._2))))
    val answer = (extrList map (x => x._1 * x._2)).sum

    answer
  }

  def binaryToHex(binary: String): String = {
    @tailrec
    def splitBy4(str: List[String], accum: List[String]): List[String] = {
      if (str.isEmpty) accum
      else splitBy4(str drop 4, (str take 4).mkString :: accum)
    }

    @tailrec
    def calculate(groupings: List[String], accum: String): String = {
      if (groupings.isEmpty) accum
      else calculate(groupings.tail, accum + extractStr(revHexToBin.get(groupings.head)))
    }

    val paddedBinary = binary.reverse + ((binary.length % 4) match {
      case 0 => ""
      case 1 => "0"
      case 2 => "00"
      case 3 => "000"
      case _ => ""
    })

    val groupedBinary = splitBy4(superSplit(paddedBinary), List()) map (_.reverse)
    val answer = calculate(groupedBinary, "")

    answer
  }

  def hexToBinary(hex: String): String = {
    @tailrec
    def calculate(num: List[String], accum: String): String = {
      if (num.length <= 0) accum
      else calculate(num.tail, accum + extractStr(hexToBinValues.get(num.head)))
    }

    val splitHex = superSplit(hex)
    val answer = calculate(splitHex, "")

    answer
  }


  def main(args: Array[String]): Unit = {
    val convType = args(0)
    val value = args(1)

    convType match {
      case "db" => println(value + " in binary: " + decimalToBinary(value))
      case "bd" => println(value + " in decimal: " + binaryToDecimal(value))
      case "dh" => println(value + " in hex: " + decimalToHex(value))
      case "hd" => println(value + " in decimal: " + hexToDecimal(value))
      case "bh" => println(value + " in hex: " + binaryToHex(value))
      case "hb" => println(value + " in binary: " + hexToBinary(value))
    }
  }
}
