import zio._
import scala.annotation.tailrec
import java.io.IOException

object SpiralMatrix extends zio.ZIOAppDefault {
  enum DIR:
    case RIGHT, DOWN, LEFT, UP
  val nextDirection: Map[DIR, DIR] = Map(
    DIR.RIGHT -> DIR.DOWN,
    DIR.DOWN -> DIR.LEFT,
    DIR.LEFT -> DIR.UP,
    DIR.UP -> DIR.RIGHT
  )
  def updateValue(currentDir: DIR): Int = currentDir match {
    case DIR.RIGHT => 1
    case DIR.DOWN => 10
    case DIR.UP => -10
    case DIR.LEFT => -1
  }
  sealed trait happyPath
  final case class happyDirection() extends happyPath
  final case class notHappyDirection() extends happyPath

  /*
   * Function: spiralTraverseMatrix
   * Given a list of rows of integers, produce a spiral traversal of the matrix represented by those rows.
   * Supported by a enumeration of directions and a utility function "updateValue" to give an scalar value
   * associated with each direction.
   * The inner function "stMatrixTR" is a tail recursive function that evaluates the state of the input
   * matrix and reduces the size of the dataset on each invocation.  The three states are:
   *
   * 1. Empty matrix: All done, return the answer.  Note that the list is in reverse as it's constructed head first.
   *
   * 2. Lookahead succeeds: This is the happy path when there is a value in the current direction of traversal. In this
   * case we drop the current position from the matrix map, update the position, keep heading in the same direction and
   * update the solution with the path value.
   *
   * 3. Lookahead fails: End of the road in this direction, we need to switch direction (given through nextDirection),
   * drop the current position from the matrix map, use a new position and update the solution with the path value.
   *
   */
  def spiralTraverseMatrix(mat: List[List[Int]]): List[Int] = {
    @tailrec
    def stMatrixTR(remainingMatrix: Map[Int,Int], pos: Int, currentDir: DIR, spiralPath: List[Int]): List[Int] = {
      val nextValue = updateValue(currentDir)
      if(remainingMatrix.isEmpty) spiralPath   // State 1: all done.
      else {
        val happyPath = remainingMatrix.contains(pos+nextValue)
        remainingMatrix.get(pos) match {
          case Some(p) if happyPath =>        // State 2: The happy path, update the answer and move on.
            stMatrixTR(remainingMatrix-pos,pos+nextValue,currentDir, p::spiralPath)

          case Some(p) if !happyPath =>       // State 3: End of the road in this direction, switch dir.
            val newPos = pos + updateValue(nextDirection(currentDir))
            stMatrixTR(remainingMatrix-pos, newPos, nextDirection(currentDir), p::spiralPath)

          case _ => spiralPath // Entry to avoid compiler warning. Shouldn't get to this case.
        }
      }
    }
    /*
    Value functionalMatrix takes a list of lists of integers representing the rows of a matrix.
    The structure of functionalMatrix is a map of the row/column -> content:
    -------------------
    11 | 12 | 13 | 14 |
    -------------------
    21 | 22 | 23 | 24 |
    -------------------
    31 | 32 | 33 | 34 |
    -------------------
    */
    val startingPosition = 11
    val functionalMatrix: Map[Int,Int] = (for {                   // Take a list of row vectors and create tuples
      a <- mat.zipWithIndex                                       // with their associate row & position. Then create
      b <- a._1.zipWithIndex                                      // a functional structure that supports a
    } yield a._2 * 10 + b._2 + startingPosition -> b._1).toMap    // relationship between elements.

    stMatrixTR(functionalMatrix, startingPosition, DIR.RIGHT, Nil).reverse   // Entry to the recursive function
  }
  // Given an m x n matrix, return all elements of the matrix in spiral order.
  /*---------------
    1 | 2 | 3 | 4 |
    ---------------
    5 | 6 | 7 | 8 |
    ---------------
    9 | 10| 11| 12|
    ---------------
  Returns:
  SpiralMatrix: List(1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 6, 7)
  */
  val inputMatrix: List[List[Int]] = List(List(1,2,3,4),List(5,6,7,8),List(9,10,11,12))

  val run: IO[IOException, Unit] =
    Console.printLine(s"SpiralMatrix: ${spiralTraverseMatrix(inputMatrix)}")

}


