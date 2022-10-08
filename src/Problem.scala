trait Problem[Input,Return] {
  val problemStatement: String
  def solution(input: Input): Return
}
