package util

import domain.GameSnap.Error._
import domain.GameSnap._
import domain._

object BoardCmdSolver {
  import util.LongBitOps._
  def solve(prev: GameSnap, next: GameSnap): Either[String, BoardCmd] = {
    val rookXors   = (prev.rooks ^ next.rooks).find1s
    val bishopXors = (prev.bishops ^ next.bishops).find1s
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
    if (prev.rooks.getBool(at)) Some(RookTaken(at))
    else if ((next.rooks & prev.bishops) == 0) Some(RookAdded(at))
    else None
  }

  private def bishopAddedOrTaken(prev: GameSnap, next: GameSnap, at: Int): Option[BoardCmd] = {
    if (prev.bishops.getBool(at)) Some(BishopTaken(at))
    else if ((next.bishops & prev.rooks) == 0) Some(BishopAdded(at))
    else None
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
