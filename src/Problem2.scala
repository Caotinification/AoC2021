object Problem2 extends Problem[Vector[String], Int]{
  val problemStatement: String = "Multiply submarine final horizontal and depth position."
  def solution(input: Vector[String]): Int = {
    val directions = input.map(s => s.split(" "))
      .map(s => (s(0), s(1).toInt))

    directions.foldLeft(0)((acc, av) => av._1 match // depth
      case "up" => acc - av._2
      case "down" => acc + av._2
      case _ => acc)
    *
    directions.collect { case ("forward", v) => v }.sum // horizontal pos
  }
  def solution2(input: Vector[String]): Int = {
    val directions =
      input.map(s => s.split(" "))
      .map(s => (s(0), s(1).toInt)) // splitting "direction number" into ("direction", number)

    val posData: (Int, Int, Int) = directions.foldLeft((0,0,0))((acc, v) => v._1 match // (depth, hPos, aim)
      case "up" => (acc._1, acc._2, acc._3 - v._2) // no change, no change in hPos, aim decreases
      case "down" => (acc._1, acc._2, acc._3 + v._2) // no change, no change in hPos, aim increases
      case "forward" => (acc._1 + v._2 * acc._3, acc._2 + v._2, acc._3) // depth += aim * hPos, hPos increases, no change
      case _ => acc)
    posData._1 * posData._2
  }
}
