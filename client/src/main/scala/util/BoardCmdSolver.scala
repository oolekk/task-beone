package util

import domain.GameSnap.Error._
import domain.GameSnap._
import domain._

object BoardCmdSolver {
  import util.LongBitOps._
  def solve(prev: GameSnap, next: GameSnap): Either[String, BoardCmd] = {
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
      if prev.moveRook(asXY(prevIndex), asXY(nextIndex)).isRight
    } yield RookMoved(prevIndex, nextIndex)
  }

  private def bishopMoved(prev: GameSnap, next: GameSnap, xors: List[Int]): Option[BoardCmd] = {
    for {
      prevIndex <- xors.find(prev.bishops.getBool)
      nextIndex <- xors.find(next.bishops.getBool)
      if prev.moveBishop(asXY(prevIndex), asXY(nextIndex)).isRight
    } yield BishopMoved(prevIndex, nextIndex)
  }

}
