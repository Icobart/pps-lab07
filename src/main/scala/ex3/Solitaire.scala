package ex3

object Solitaire extends App:
  type Number = (Int, Int)
  type Solution = Iterable[Number]
  type IterableFactory = Solution => Iterable[Solution]
  given IterableFactory = LazyList(_)

  def placeMarks(n: Int, width: Int, height: Int)(using factory: IterableFactory): Iterable[Solution] = n match
    case 0 => factory(List())
    case _ =>
      for
        numbers <- placeMarks(n - 1, width, height)
        x <- 0 until width
        y <- 0 until height
        number = (x, y)
        if isValidMove(number, numbers, width, height)
        if numbers.nonEmpty || number == (width / 2, height / 2)
      yield
        number :: numbers.toList

  def isValidMove(nextPosition: Number, currentSolution: Solution, width: Int, height: Int): Boolean =
    val insideBounds = nextPosition._1 < width && nextPosition._1 >= 0 && nextPosition._2 < height && nextPosition._2 >= 0
    val freePosition = !currentSolution.exists(p => p == nextPosition)
    val isValidJump = currentSolution.headOption match
      case None => true
      case Some(lastPosition) => isCorrectJump(lastPosition, nextPosition)
    isValidJump && insideBounds && freePosition

  def isCorrectJump(from: Number, to: Number): Boolean =
    val dx = (from._1 - to._1).abs
    val dy = (from._2 - to._2).abs
    ((dx == 3 && dy == 0) || (dx == 0 && dy == 3)) || (dx == 2 && dy == 2)

  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def printSolution(si: (Solution, Int), width: Int, height: Int): Unit =
    println()
    println(s"Solution: ${si._2 + 1}:")
    println(render(si._1.toSeq, width, height))

  val w = 5
  val h = 5
  val totalMoves = w * h
  placeMarks(totalMoves, w, h)
    .zipWithIndex
    .foreach(si => printSolution(si, w, h))