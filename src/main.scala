@main def main: Unit = {
  val inputs = io.Source.fromFile("C:\\Users\\Justin\\IdeaProjects\\Reminders\\inputs\\p3.txt").getLines.toVector
  println(Problem3.solution2(inputs))
}