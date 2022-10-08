import scala.annotation.tailrec

object Problem3 extends Problem[Vector[String], Int]{
  private def binaryToInt(b: String): Int = Integer.parseInt(b, 2)

  override val problemStatement: String = "Find power consumption of submarine"

  def findMaxMinBit(input: Vector[String]): (String, String) = {
    val transposal = input.transpose // rows become columns, columns become rows
    val maxmin = transposal.map(s => { // Vector[(Common, Rare)]
      val ones = s.count(_ == '1') // count 1s
      val zeroes = s.length - ones // count 0s
      (if ones == zeroes then "1" else if Math.max(ones, zeroes) == ones then "1" else "0",
        if ones == zeroes then "0" else if Math.min(ones, zeroes) == ones then "1" else "0")
    })
    val gamma = maxmin.foldLeft("")((acc, v) => acc + v._1)
    val epsilon = maxmin.foldLeft("")((acc, v) => acc + v._2)
    (gamma, epsilon)
  }

  def solution(input: Vector[String]): Int = {
    val (gamma, epsilon) = findMaxMinBit(input)
    binaryToInt(gamma) * binaryToInt(epsilon)
  }
  def solution2(input: Vector[String]): Int = {
    def filterByBits(binaryStrings: Vector[String], mode: Boolean, pos: Int = 0): String = {
      val (max, min) = findMaxMinBit(binaryStrings)
      val selection = if mode then max else min
      val matched = binaryStrings.filter(_(pos) == selection(pos))

      if matched.length == 1 then matched.head // this single element is our result
      else filterByBits(matched, mode, pos + 1)
    }
    val generator = binaryToInt(filterByBits(input, true))
    val scrubber = binaryToInt(filterByBits(input, false))
    generator * scrubber
  }
}
