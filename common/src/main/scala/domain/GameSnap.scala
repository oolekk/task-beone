package domain

import util.LongBitOps._
import zio.ZIO
import zio.json._

import scala.util.Random


case class GameSnap private (rs: Long, bs: Long) {

  // 64 bits of Long correspond to 64 fields of the game board.
  // Two Long values suffice to represent exact board state.
  // One long for rooks present-or-not on a field, second for bishops.
  // Storage, checks and transfer of this simple data is efficient.
  // For transfer and storage, this can be serialized as hex

  // Data is very compact so each move is best encoded as simply
  // the next GameSnap, rather than move, it is also sufficient to
  // restore last board state, without need to replay all the moves.
  // Bitwise checks on snaps allow quick & easy comparison / validation.

  // For more general case, we could have array of Long to represent
  // larger variety of pieces. For game board of different size we can
  // use byte array instead of Long. For specific scenario we have at
  // hand here the above representation of game state is a good fit.

}

object GameSnap {

  val SIZE: Int = 8

  // immutable, can be re-used safely
  val empty: GameSnap = GameSnap(0, 0)
  def apply(rooks: Long, bishops: Long): GameSnap = {
    // Only valid snap can be created, so we don't have to validate or worry about it later on.
    assert(!rooks.overlaps(bishops), Error.CORRUPT_SNAP_MSG)
    new GameSnap(rooks, bishops)
  }

  object Error {
    val XY_OUT_OF_RANGE_MSG: String       = "XY out of range!"
    val FIELD_NOT_VOID_MSG: String        = "Field not empty!"
    val ROOK_NOT_THERE_MSG: String        = "Rook not there!"
    val BISHOP_NOT_THERE_MSG: String      = "Bishop not there!"
    val ROOK_CANNOT_MOVE_THERE: String    = "Rook cannot move there!"
    val BISHOP_CANNOT_MOVE_THERE: String  = "Bishop cannot move there!"
    val INVALID_ROOK_PLACEMENT: String    = "Invalid rook placement!"
    val INVALID_BISHOP_PLACEMENT: String  = "Invalid bishop placement!"
    val PIECE_NOT_THERE_MSG: String       = "Piece not there!"
    val INVALID_GAME_BOARD_ACTION: String = "Change not feasible on current board!"

    val CORRUPT_SNAP_MSG     = "Corrupt snap, two different pieces are not allowed on one field!"
    val INVALID_SNAP_HEX_MSG = "Parsing of GameSnap from invalid hex string has failed!"
  }


  implicit val encoder: JsonEncoder[GameSnap] = DeriveJsonEncoder.gen[GameSnap]
  implicit val decored: JsonDecoder[GameSnap] = DeriveJsonDecoder.gen[GameSnap]


  def zioFromHex(hex: String): ZIO[Any, SnapError, GameSnap] = {
    val (rsHex, bsHex) = hex.splitAt(16)
    for {
      rs <- fromHexStr(rsHex)
      bs <- fromHexStr(bsHex)
      if !rs.overlaps(bs)
    } yield ZIO.succeed(GameSnap(rs, bs))
  } getOrElse ZIO.fail(InvalidSnapHex)

  val zioEmpty: ZIO[Any, SnapError, GameSnap] = zio(0, 0)
  def zio(rooks: Long, bishops: Long): ZIO[Any, SnapError, GameSnap] = {
    if (rooks.overlaps(bishops)) ZIO.fail(CorruptSnap$)
    else ZIO.succeed(new GameSnap(rooks, bishops))
  }

  def gen(): GameSnap = {
    val bs = Random.nextLong()
    val rs = Random.nextLong()
    new GameSnap(rs not bs, bs not rs)
  }

  def asXY(i: Int): (Int, Int) = (i % 8, i / 8)

  private def rawIndex(xy: (Int, Int)): Int = xy._1 + SIZE * xy._2

  def rectLine(xor: Long): List[Int] = xor.find1s match {
    case List(i, j) if i / SIZE == j / SIZE => (i to j).toList // on same row
    case List(i, j) if i % SIZE == j % SIZE => (i to j by SIZE).toList // on same col
    case _ => Nil
  }

  def diagLine(xor: Long): List[Int] = xor.find1s match {
    // TRICKY: check the \ variant before / because 7 * 9 == 63 would match wrong diagonal
    case List(i, j) if (j - i) % (SIZE + 1) == 0 => (i to j by (SIZE + 1)).toList // on \ diagonal
    case List(i, j) if (j - i) % (SIZE - 1) == 0 => (i to j by (SIZE - 1)).toList // on / diagonal
    case _ => Nil
  }

  implicit class GameSnapOps(snap: GameSnap) {

    def asHex: Hex       = Hex(asHexStr)
    def asHexStr: String = s"${snap.rooks.hexString}${snap.bishops.hexString}"

    def allPieces: Long = snap.rooks | snap.bishops

    def rooks: Long   = snap.rs // When bit is 1 that field has a rook
    def bishops: Long = snap.bs // When bit is 1 that field has a bishop

    def isRook(xy: (Int, Int)): Boolean =
      bitIndex(xy).exists(snap.rooks.getBool)

    def isBishop(xy: (Int, Int)): Boolean =
      bitIndex(xy).exists(snap.bishops.getBool)

    def isVoid(xy: (Int, Int)): Boolean =
      !bitIndex(xy).exists(allPieces.getBool)

    // Each valid action produces new GameSnap

    def forceRook(xy: (Int, Int)): GameSnap = {
      val index = rawIndex(xy)
      GameSnap(snap.rooks.setBitTo1(index), snap.bishops.setBitTo0(index))
    }
    def forceBishop(xy: (Int, Int)): GameSnap = {
      val index = rawIndex(xy)
      GameSnap(snap.rooks.setBitTo0(index), snap.bishops.setBitTo1(index))
    }

    def moveRook(xyOld: (Int, Int), xyNew: (Int, Int)): Either[String, GameSnap] =
      snap.takeRook(xyOld).flatMap(_.addRook(xyNew)).flatMap { nextSnap =>
        Either.cond(
          // line from old xy to new xy must be empty except one: old xy
          rectLine(snap.rooks xor nextSnap.rooks).count(allPieces.getBool) == 1,
          nextSnap,
          Error.ROOK_CANNOT_MOVE_THERE
        )
      }

    def moveBishop(xyPrev: (Int, Int), xyNext: (Int, Int)): Either[String, GameSnap] =
      snap.takeBishop(xyPrev).flatMap(_.addBishop(xyNext)).flatMap { nextSnap =>
        Either.cond(
          // line from old xy to new xy must be empty except one: old xy
          diagLine(snap.bishops xor nextSnap.bishops).count(allPieces.getBool) == 1,
          nextSnap,
          Error.BISHOP_CANNOT_MOVE_THERE
        )
      }

    def movePiece(xyPrev: (Int, Int), xyNext: (Int, Int)): Either[String, GameSnap] = {
      if (isBishop(xyPrev)) moveBishop(xyPrev, xyNext)
      else if (isRook(xyPrev)) moveRook(xyPrev, xyNext)
      else Left(Error.PIECE_NOT_THERE_MSG)
    }

    def addRook(xy: (Int, Int)): Either[String, GameSnap] = voidIndex(xy)
      .map(i => GameSnap(snap.rooks.setBitTo1(i), snap.bishops))

    def addBishop(xy: (Int, Int)): Either[String, GameSnap] = voidIndex(xy)
      .map(i => GameSnap(snap.rooks, snap.bishops.setBitTo1(i)))

    def takeRook(xy: (Int, Int)): Either[String, GameSnap] =
      if (snap.isRook(xy)) Right(GameSnap(snap.rooks.setBitTo0(rawIndex(xy)), snap.bishops))
      else Left(Error.ROOK_NOT_THERE_MSG)

    def takeBishop(xy: (Int, Int)): Either[String, GameSnap] =
      if (snap.isBishop(xy)) Right(GameSnap(snap.rooks, snap.bishops.setBitTo0(rawIndex(xy))))
      else Left(Error.BISHOP_NOT_THERE_MSG)

    def takePiece(xy: (Int, Int)): Either[String, GameSnap] =
      takeRook(xy) orElse takeBishop(xy) match {
        case Left(_) => Left(Error.PIECE_NOT_THERE_MSG)
        case v       => v
      }

    private def bitIndex(xy: (Int, Int)): Either[String, Int] = {
      val (x, y) = xy
      Either.cond(x >= 0 && x < SIZE && y >= 0 && y < SIZE, rawIndex(xy), Error.XY_OUT_OF_RANGE_MSG)
    }

    private def voidIndex(xy: (Int, Int)): Either[String, Int] = bitIndex(xy)
      .flatMap(i => Either.cond(!snap.bishops.getBool(i) && !snap.rooks.getBool(i), i, Error.FIELD_NOT_VOID_MSG))
  }

}
