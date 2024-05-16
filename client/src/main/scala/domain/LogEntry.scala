package domain

import domain.GameSnap.asXY
import domain.LogEntry.{OFF_BOARD_SUFFIX, ON_BOARD_SUFFIX, RoundXY}

object LogEntry {
  val ON_BOARD_SUFFIX  = "Still on the board."
  val OFF_BOARD_SUFFIX = "Taken off the board."

  case class Bishop(id: Int, firstAt: RoundXY, lastAt: RoundXY, moves: List[Int]) extends LogEntry {
    def describe(round: Int, onBoard: Boolean): String = describe(s"Id:$id Bishop ", round, onBoard)
  }
  case class Rook(id: Int, firstAt: RoundXY, lastAt: RoundXY, moves: List[Int]) extends LogEntry {
    def describe(round: Int, onBoard: Boolean): String = describe(s"Id:$id Rook ", round, onBoard)
  }
  case class RoundXY(round: Int, at: Int) {
    def xy: (Int, Int) = asXY(at)
  }
}

sealed trait LogEntry {
  val id: Int
  val moves: List[Int] // from newest-to-oldest is left-to-right
  val firstAt, lastAt: RoundXY
  def describe(round: Int, onBoard: Boolean): String
  def describe(prefix: String, round: Int, onBoard: Boolean): String = {
    val rounds = if (onBoard) round - firstAt.round else lastAt.round - firstAt.round
    s"${prefix}spent ${rounds} rounds on the game board." +
      s" Was first put on the board in round ${firstAt.round} on ${asXY(firstAt.at)}." +
      s" Moved ${moves.size - 1} times for total distance of $distance." +
      s" Last touched in round ${lastAt.round} on ${asXY(lastAt.at)}. " +
      (if (onBoard) ON_BOARD_SUFFIX else OFF_BOARD_SUFFIX)
  }

  private def distance: Int = (moves.sliding(2, 1) collect { case List(i, j) =>
    if (i / 8 == j / 8) (j - i).abs            // horizontal distance
    else if ((j - i) % 9 == 0) (j - i).abs / 9 // diagonal \\ distance
    else if (i % 8 == j % 8) (j - i).abs / 8   // vertical distance
    else (j - i).abs / 7                       // diagonal // distance
  }).sum
}
