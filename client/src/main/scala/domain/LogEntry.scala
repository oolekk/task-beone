package domain

import domain.GameSnap.asXY
import domain.LogEntry._

object LogEntry {
  private val ON_BOARD_SUFFIX  = "Still on the board."
  private val OFF_BOARD_SUFFIX = "Taken off the board."

  case class RoundXY(round: Int, at: Int)

  case class Bishop(id: Int, firstAt: RoundXY, lastAt: RoundXY, moves: List[Int]) extends LogEntry {
    def moved(round: Int, at: Int): Bishop = copy(lastAt = RoundXY(round, at), moves = at :: moves)

    def taken(round: Int): Bishop = copy(lastAt = lastAt.copy(round = round))

    def describe(round: Int, onBoard: Boolean): String = describe(s"Id:$id Bishop ", round, onBoard)

  }

  case class Rook(id: Int, firstAt: RoundXY, lastAt: RoundXY, moves: List[Int]) extends LogEntry {
    def moved(round: Int, at: Int): Rook = copy(lastAt = RoundXY(round, at), moves = at :: moves)

    def taken(round: Int): Rook = copy(lastAt = lastAt.copy(round = round))

    def describe(round: Int, onBoard: Boolean): String = describe(s"Id:$id Rook ", round, onBoard)
  }
}

sealed trait LogEntry {
  val id: Int
  val moves: List[Int] // from newest-to-oldest is left-to-right
  val firstAt, lastAt: RoundXY
  def describe(round: Int, onBoard: Boolean): String
  def moved(round: Int, at: Int): LogEntry
  def taken(round: Int): LogEntry

  private def distance: Int = (moves.sliding(2, 1) collect { case List(i, j) =>
    if (i / 8 == j / 8) (j - i).abs            // horizontal distance
    else if ((j - i) % 9 == 0) (j - i).abs / 9 // diagonal \\ distance
    else if (i % 8 == j % 8) (j - i).abs / 8   // vertical distance
    else (j - i).abs / 7                       // diagonal // distance
  }).sum

  def describe(prefix: String, round: Int, onBoard: Boolean): String = {
    val rounds = if (onBoard) round - firstAt.round else lastAt.round - firstAt.round
    s"${prefix}spent $rounds rounds on the game board." +
      s" Was first put on the board in round ${firstAt.round} on ${asXY(firstAt.at)}." +
      s" Moved ${moves.size - 1} times for total distance of $distance." +
      s" Last touched in round ${lastAt.round} on ${asXY(lastAt.at)}. " +
      (if (onBoard) ON_BOARD_SUFFIX else OFF_BOARD_SUFFIX)
  }
}
