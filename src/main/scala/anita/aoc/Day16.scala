package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day16 {
  sealed class Packet(val version: Int) {
    def reduce[B](f: ValuePacket => B)(g: (OperatorPacket, List[B]) => B): B = this match {
      case packet: OperatorPacket => g(packet, packet.packets.map(_.reduce(f)(g)))
      case packet: ValuePacket => f(packet)
    }
  }

  class OperatorPacket(version: Int, val operatorType: Int, val packets: List[Packet]) extends Packet(version) {
    override def toString: String = s"OperatorPacket(version=$version, operatorType=$operatorType, packets=$packets)"
  }

  class ValuePacket(version: Int, val value: Long) extends Packet(version) {
    override def toString: String = s"ValuePacket(version=$version, value=$value)"
  }

  def part1(packet: Packet): Int = {
    packet.reduce(_.version)((p, l) => p.version + l.sum)
  }

  def part2(packet: Packet): Long = {
    packet.reduce(_.value)((p, l) => p.operatorType match {
      case 0 => l.sum
      case 1 => l.product
      case 2 => l.min
      case 3 => l.max
      case 5 => if (l.head < l.last) 1L else 0L
      case 6 => if (l.head > l.last) 1L else 0L
      case 7 => if (l.head == l.last) 1L else 0L
    })
  }

  def main(args: Array[String]): Unit = {
    val bits = parseInput()
    println(part1(bits))
    println(part2(bits))
  }

  def parseInput(): Packet = {
    parsePacket(getInput(16).head.map(c => c.toString.toInt(16).toBinaryString.padStart(4, '0')).mkString)._1
  }

  val packetRegex = new Regex("(.{3})(.{3})(.*)")

  def parsePacket(bits: String): (Packet, String) = bits match {
    case packetRegex(version, "100", content) =>
      val (value, unparsed) = parseValue(content)
      (new ValuePacket(version.toInt(2), value), unparsed)
    case packetRegex(version, operatorType, content) if content.head == '0' =>
      val length = content.tail.take(15).toInt(2)
      val (packets, _) = parsePackets(content.slice(16, length + 16))
      (new OperatorPacket(version.toInt(2), operatorType.toInt(2), packets), content.drop(length + 16))
    case packetRegex(version, operatorType, content) =>
      val number = content.tail.take(11).toInt(2)
      val (packets, unparsed) = parsePackets(content.drop(12), number)
      (new OperatorPacket(version.toInt(2), operatorType.toInt(2), packets), unparsed)
  }

  val valueRegex = new Regex("((?:1.{4})*)(0.{4})(.*)")

  def parseValue(content: String): (Long, String) = content match {
    case valueRegex(leadingParts, lastPart, unparsed) =>
      ((leadingParts.grouped(5).map(_.tail).mkString + lastPart.tail).toLong(2), unparsed)
  }

  def parsePackets(content: String, numberPackets: Int = -1): (List[Packet], String) = {
    @tailrec
    def go(acc: List[Packet], s: String, n: Int = -1): (List[Packet], String) = {
      if (n == 0 || s.forall(_ == '0')) (acc, s)
      else {
        val (packet, unparsed) = parsePacket(s)
        go(packet :: acc, unparsed, n - 1)
      }
    }

    go(List.empty, content, numberPackets)
  }
}
