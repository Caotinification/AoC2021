object Problem1 extends Problem[Vector[Int], Int] {
  val problemStatement: String = "Find how many times a submarine increases in depth"
  def solution(input: Vector[Int]): Int = input.sliding(2).count(a => a(1) > a(0))
  def solution2(input: Vector[Int]): Int = input.sliding(3).map(_.sum).sliding(2).count(a => a(1) > a(0))
}
