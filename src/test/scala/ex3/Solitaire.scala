package ex3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolitaireTest extends AnyFlatSpec with Matchers:

  "isCorrectJump" should "allow orthogonal jumps of distance 3" in:
    Solitaire.isCorrectJump((3, 3), (0, 3)) should be (true)
    Solitaire.isCorrectJump((3, 3), (6, 3)) should be (true)
    Solitaire.isCorrectJump((3, 3), (3, 0)) should be (true)
    Solitaire.isCorrectJump((3, 3), (3, 6)) should be (true)

  it should "allow diagonal jumps of distance 2" in:
    Solitaire.isCorrectJump((3, 3), (1, 1)) should be (true)
    Solitaire.isCorrectJump((3, 3), (5, 5)) should be (true)

  it should "reject invalid jumps" in:
    Solitaire.isCorrectJump((3, 3), (2, 2)) should be (false)
    Solitaire.isCorrectJump((3, 3), (3, 4)) should be (false)
    Solitaire.isCorrectJump((3, 3), (0, 0)) should be (false)

  "isValidMove" should "reject out of bounds moves" in:
    Solitaire.isValidMove((5, 5), List(), 5, 5) should be (false)
    Solitaire.isValidMove((-1, 2), List(), 5, 5) should be (false)

  it should "reject already visited cells" in:
    Solitaire.isValidMove((2, 2), List((2, 2)), 5, 5) should be (false)

  it should "accept a valid jump" in:
    Solitaire.isValidMove((0, 3), List((3, 3)), 5, 5) should be (true)