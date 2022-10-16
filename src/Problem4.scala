object Problem4 extends Problem[Vector[String], Int]{
  val problemStatement = ""
  private type BingoBoard = Vector[Vector[Int]]
  private def getBingoBoards(input: Vector[String]): Vector[BingoBoard] = {
    val matrices = input.grouped(5)
    val bingoBoard5x5 =
      matrices.map( // for each group of 5...
          _.map( // for each item in a group ...
            _.split("\\s+") // numbers in a row are separated by 1-2 spaces
              .filter(_!="") // remove empty strings. for some reason the previous computation leaves them behind
              .map(_.toInt).toVector)).toVector // finally, convert every number string into an int
    bingoBoard5x5
  }
  def solution(input: Vector[String]): Int = {
    val bingoes = input(0).split(",").map(_.toInt).toSet
    val numberRows = input.tail
    val bingoBoards = getBingoBoards(numberRows)
    val booleanMatrix = bingoBoards.map(
      board => board.map(
        row => row.toSet & bingoes))
    1
  }
}
