package domain

import domain.GameSnap.Error._
import zio.ZIO
import zio.json._
import zio.kafka.serde.Serde

case class BishopAdded(at: Int) extends BoardCmd {
  override def invert: BoardCmd = BishopTaken(at)
}
case class BishopTaken(at: Int) extends BoardCmd {

  override def invert: BoardCmd = BishopAdded(at)
}
case class BishopMoved(from: Int, to: Int) extends BoardCmd {
  override def invert: BoardCmd = BishopMoved(to, from)
}

case class RookAdded(at: Int) extends BoardCmd {
  override def invert: BoardCmd = RookTaken(at)
}
case class RookTaken(at: Int) extends BoardCmd {
  override def invert: BoardCmd = RookAdded(at)
}
case class RookMoved(from: Int, to: Int) extends BoardCmd {
  override def invert: BoardCmd = RookMoved(to, from)
}

sealed trait BoardCmd {
  def invert: BoardCmd
}

object BoardCmd {

  implicit val encoder: JsonEncoder[BoardCmd] = DeriveJsonEncoder.gen[BoardCmd]
  implicit val decoder: JsonDecoder[BoardCmd] = DeriveJsonDecoder.gen[BoardCmd]

  val serde: Serde[Any, BoardCmd] = Serde.string.inmapM(string =>
    ZIO
      .fromEither(string.fromJson[BoardCmd])
      .mapError(msg => new RuntimeException(msg))
  )(cmd => ZIO.from(cmd.toJson))

  private val INVALID_ROOK_PLACEMENT    = "Invalid rook placement!"
  private val INVALID_BISHOP_PLACEMENT  = "Invalid bishop placement!"
  private val INVALID_GAME_BOARD_ACTION = "Invalid game board action!"

  import util.LongBitOps._
  def resolve(prev: GameSnap, next: GameSnap): Either[String, BoardCmd] = {
    val rookXors   = prev.rooks.xor(next.rooks).find1s
    val bishopXors = prev.bishops.xor(next.bishops).find1s
    (rookXors, bishopXors) match {
      case (List(at), Nil) =>
        rookAddedOrTaken(prev, next, at).toRight(INVALID_ROOK_PLACEMENT)
      case (Nil, List(at)) =>
        bishopAddedOrTaken(prev, next, at).toRight(INVALID_BISHOP_PLACEMENT)
      case (List(_, _), Nil) =>
        rookMoved(prev, next, rookXors).toRight(ROOK_CANNOT_MOVE_THERE)
      case (Nil, List(_, _)) =>
        bishopMoved(prev, next, bishopXors).toRight(BISHOP_CANNOT_MOVE_THERE)
      case _ => Left(INVALID_GAME_BOARD_ACTION)
    }
  }

  private def rookAddedOrTaken(prev: GameSnap, next: GameSnap, at: Int): Option[BoardCmd] = {
    if (next.rooks.overlaps(prev.bishops)) None
    else if (prev.rooks.getBool(at)) Some(RookTaken(at))
    else Some(RookAdded(at))
  }

  private def bishopAddedOrTaken(prev: GameSnap, next: GameSnap, at: Int): Option[BoardCmd] = {
    if (next.bishops.overlaps(prev.rooks)) None
    else if (prev.bishops.getBool(at)) Some(BishopTaken(at))
    else Some(BishopAdded(at))
  }

  private def rookMoved(prev: GameSnap, next: GameSnap, xors: List[Int]): Option[BoardCmd] = {
    for {
      prevIndex <- xors.find(prev.rooks.getBool)
      nextIndex <- xors.find(next.rooks.getBool)
      if prev.moveRook(prevIndex, nextIndex).isRight
    } yield RookMoved(prevIndex, nextIndex)
  }

  private def bishopMoved(prev: GameSnap, next: GameSnap, xors: List[Int]): Option[BoardCmd] = {
    for {
      prevIndex <- xors.find(prev.bishops.getBool)
      nextIndex <- xors.find(next.bishops.getBool)
      if prev.moveBishop(prevIndex, nextIndex).isRight
    } yield BishopMoved(prevIndex, nextIndex)
  }
}
