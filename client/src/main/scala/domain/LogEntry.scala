package domain

import domain.GameSnap.asXY
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class Bishop(id: Int, firstAt: RoundXY, lastAt: RoundXY, moves: List[Int]) extends LogEntry {
  def describe(suffix: String): String = describe(s"Id:$id Bishop ", suffix)
}
case class Rook(id: Int, firstAt: RoundXY, lastAt: RoundXY, moves: List[Int]) extends LogEntry {
  def describe(suffix: String): String = describe(s"Id:$id Rook ", suffix)
}
case class RoundXY(round: Int, at: Int) {
  def xy: (Int, Int) = asXY(at)
}

sealed trait LogEntry {
  val id: Int
  val moves: List[Int] // from newest-to-oldest is left-to-right
  val firstAt, lastAt: RoundXY
  def describe(prefix: String): String
  def describe(prefix: String, suffix: String): String =
    s"${prefix}spent ${lastAt.round - firstAt.round} rounds on the game board." +
      s" Was first put on the board in round ${firstAt.round} on ${asXY(firstAt.at)}." +
      s" Moved ${moves.size - 1} times for total distance of $distance." +
      s" Last seen in round ${lastAt.round} on ${asXY(lastAt.at)}.$suffix"

  def distance: Int = (moves.sliding(2, 1) collect { case List(i, j) =>
    if (i / 8 == j / 8) (j - i).abs            // horizontal distance
    else if ((j - i) % 9 == 0) (j - i).abs / 9 // diagonal \\ distance
    else if (i % 8 == j % 8) (j - i).abs / 8   // vertical distance
    else (j - i).abs / 7                       // diagonal // distance
  }).sum
}

object LogEntry {
  implicit val entryEnc: JsonEncoder[LogEntry]  = DeriveJsonEncoder.gen[LogEntry]
  implicit val entryDec: JsonDecoder[LogEntry]  = DeriveJsonDecoder.gen[LogEntry]
  implicit val gameLogEnc: JsonEncoder[GameLog] = DeriveJsonEncoder.gen[GameLog]
  implicit val gameLogDec: JsonDecoder[GameLog] = DeriveJsonDecoder.gen[GameLog]
  implicit val bishopEnc: JsonEncoder[Bishop]   = DeriveJsonEncoder.gen[Bishop]
  implicit val bishopDec: JsonDecoder[Bishop]   = DeriveJsonDecoder.gen[Bishop]
  implicit val rookEnc: JsonEncoder[Rook]       = DeriveJsonEncoder.gen[Rook]
  implicit val rookDec: JsonDecoder[Rook]       = DeriveJsonDecoder.gen[Rook]
  implicit val roundXYenc: JsonEncoder[RoundXY] = DeriveJsonEncoder.gen[RoundXY]
  implicit val roundXYdec: JsonDecoder[RoundXY] = DeriveJsonDecoder.gen[RoundXY]
}
