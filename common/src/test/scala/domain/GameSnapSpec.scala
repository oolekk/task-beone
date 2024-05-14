package domain

import domain.GameSnap.Error._
import zio.test._

object GameSnapSpec extends ZIOSpecDefault {

  import scala.util.Try
  import domain.GameSnap._
  import util.LongBitOps._

  def spec: Spec[Any, Nothing] = suite("GameSnapSpec")(
    test("constructor must always succeed for non-overlapping set of rooks and bishops") {
      assertTrue(
        !(1 to 1000)
          .map { _ =>
            val rs      = scala.util.Random.nextLong()
            val bs      = scala.util.Random.nextLong()
            val overlap = bs & rs
            Try {
              GameSnap(rs & ~overlap, bs)
              GameSnap(rs, bs & ~overlap)
              GameSnap(rs & ~overlap, bs & ~overlap)
            }
          }
          .exists(_.isFailure)
      )
    },
    test("constructor must fail for overlapping set of rooks and bishops, succeed otherwise") {
      // There will most likely be some overlap for randomly assigned set of rooks and bishops
      // so this case is mostly for testing creation of such corrupt snaps won't succeed.
      // On rare occasion it may be successful, but that case is better checked in dedicated test.
      val trySnaps: Seq[(Boolean, Try[GameSnap])] = for {
        _ <- (1 to 1000).toList
        rs         = scala.util.Random.nextLong()
        bs         = scala.util.Random.nextLong()
        hasOverlap = (bs & rs) != 0
      } yield (hasOverlap, Try { GameSnap(rs, bs) })
      assertTrue(
        trySnaps.forall {
          case (hasOverlap, trySnap) =>
            hasOverlap && trySnap.isFailure || !hasOverlap && trySnap.isSuccess
        }
      )
    },
    test("generator must produce non-corrupt game snaps") {
      assertTrue(
        !(1 to 1000)
          .map(_ => GameSnap.gen())
          // no corrupt snap with bishop and rook on the same field should be generated
          .exists(snap => (snap.rooks & snap.bishops) != 0)
      )
    },
    test("rooks indicates fields with rooks via non-zero bit") {
      val (r1, b1, r2) = ((3, 5), (7, 7), (4, 6))
      val Right(snap) =
        GameSnap.empty
          .addRook(r1)
          .flatMap(_.addBishop(b1))
          .flatMap(_.addRook(r2))

      assertTrue(snap.rooks.find1s.map(asXY) == List(r1, r2))
    },
    test("bishops indicates fields with bishops via non-zero bit") {
      val (b1, r1, b2) = ((3, 5), (7, 7), (4, 6))
      val Right(snap) = GameSnap.empty
        .addBishop(b1)
        .flatMap(_.addRook(r1))
        .flatMap(_.addBishop(b2))

      assertTrue(snap.bishops.find1s.map(asXY) == List((3, 5), (4, 6)))
    },
    test("allPieces indicates all non-empty fields via non-zero bit") {
      val (b1, r1, b2) = ((3, 5), (7, 7), (4, 6))
      val Right(snap) = GameSnap.empty
        .addBishop(b1)
        .flatMap(_.addRook(r1))
        .flatMap(_.addBishop(b2))

      assertTrue(snap.allPieces.find1s.map(asXY) == List((3, 5), (4, 6), (7, 7)))
    },
    test("addRook adds rook only to empty field within board") {
      val (r1, b1) = ((3, 6), (2, 2))

      val Right(snap) = GameSnap.empty
        .addRook(r1)
        .flatMap(_.addBishop(b1))

      assertTrue(
        snap.isRook(r1),
        snap.isBishop(b1),
        snap.addRook(r1) == Left(FIELD_NOT_VOID_MSG),    // contains rook already
        snap.addRook(b1) == Left(FIELD_NOT_VOID_MSG),    // contains bishop
        snap.addRook(4, 5).isRight,                      // available
        snap.addRook(2, 8) == Left(NO_SUCH_FIELD_MSG), // 8 is beyond board size
        snap.addRook(8, 8) == Left(NO_SUCH_FIELD_MSG), // even more so
        snap.addRook(-3, 2) == Left(NO_SUCH_FIELD_MSG) // must be 0-7
      )
    },
    test("addBishop adds bishop only to empty field within board") {
      val (b1, r1) = ((3, 6), (2, 2))

      val Right(snap) = GameSnap.empty
        .addBishop(3, 6)
        .flatMap(_.addRook(2, 2))

      assertTrue(
        snap.isBishop(b1),
        snap.isRook(r1),
        snap.addBishop(b1) == Left(FIELD_NOT_VOID_MSG),    // contains bishop already
        snap.addBishop(r1) == Left(FIELD_NOT_VOID_MSG),    // contains rook already
        snap.addBishop(4, 5).isRight,                      // available
        snap.addBishop(2, 8) == Left(NO_SUCH_FIELD_MSG), // 8 is beyond board size
        snap.addBishop(8, 8) == Left(NO_SUCH_FIELD_MSG), // even more soe
        snap.addBishop(-3, 2) == Left(NO_SUCH_FIELD_MSG) // must be 0-7
      )
    },
    suite("taking pieces off the board")(
      test("takeRook takes away a previously placed rook") {
        val (r1, b1, r2) = ((3, 5), (7, 7), (4, 6))
        val Right(snap) = GameSnap.empty
          .addRook(r1)
          .flatMap(_.addBishop(b1))
          .flatMap(_.addRook(r2))

        assertTrue(
          snap.rooks.find1s.map(asXY) == List(r1, r2),         // here are the rooks
          snap.takeRook(r1).isRight,                           // available and taken
          snap.isRook(r2),                                     // it was there ...
          snap.takeRook(r2).map(_.isRook(r2)).contains(false), // it is gone now
          snap.takeRook(2, 4) == Left(ROOK_NOT_THERE_MSG),     // there is no rook there
          snap.takeRook(b1) == Left(ROOK_NOT_THERE_MSG)        // there is bishop not rook there
        )
      },
      test("takeBishop takes away a previously placed bishop") {
        val (b1, r1, b2) = ((3, 5), (7, 7), (4, 6))
        val Right(snap) = GameSnap.empty
          .addBishop(b1)
          .flatMap(_.addRook(r1))
          .flatMap(_.addBishop(b2))

        assertTrue(
          snap.bishops.find1s.map(asXY) == List(b1, b2),           // here are the bishops
          snap.takeBishop(b1).isRight,                             // available and taken
          snap.isBishop(b2),                                       // it was there ...
          snap.takeBishop(b2).map(_.isBishop(b2)).contains(false), // it is gone now
          snap.takeBishop(2, 4) == Left(BISHOP_NOT_THERE_MSG),     // there is no rook there
          snap.takeBishop(r1) == Left(BISHOP_NOT_THERE_MSG)        // there is rook not bishop there
        )

      },
      test("takePiece takes away a previously placed piece") {
        val (r1, b1, r2, b2) = ((5, 3), (4, 6), (3, 5), (7, 7))
        val Right(snap) = GameSnap.empty
          .addRook(r1)
          .flatMap(_.addBishop(b1))
          .flatMap(_.addRook(r2))
          .flatMap(_.addBishop(b2))

        assertTrue(
          snap.bishops.find1s.map(asXY) == List(b1, b2),       // here are the bishops
          snap.takePiece(b1).isRight,                          // available and taken
          snap.isBishop(b2),                                   // it was there ...
          snap.takePiece(b2).map(_.isVoid(b2)).contains(true), // it is gone now
          snap.rooks.find1s.map(asXY) == List(r1, r2),         // here are the rooks
          snap.takePiece(r1).isRight,                          // available and taken
          snap.isRook(r2),                                     // it was there ...
          snap.takePiece(r2).map(_.isVoid(r2)).contains(true), // it is gone now
          snap.takePiece(2, 4) == Left(PIECE_NOT_THERE_MSG)    // there is no piece there
        )
      }
    ),
    test("isRook indicates if field is occupied by a rook") {

      val (r1, b1, b2, r2) = ((3, 5), (7, 7), (5, 2), (4, 6))

      val Right(snap) = GameSnap.empty
        .addRook(r1)
        .flatMap(_.addBishop(b1))
        .flatMap(_.addBishop(b2))
        .flatMap(_.addRook(r2))

      assertTrue(
        !snap.isRook(1, 1), // nothing there
        !snap.isRook(b1),   // bishop there, not rook
        snap.isRook(r1),    // it is there
        snap.isRook(r2),    // and here too
        !snap.isRook(b2)    // but not here
      )
    },
    test("isBishop indicates if field is occupied by a bishop") {

      val (r1, b1, b2, r2) = ((3, 5), (7, 7), (5, 2), (4, 6))

      val Right(snap) = GameSnap.empty
        .addRook(r1)
        .flatMap(_.addBishop(b1))
        .flatMap(_.addBishop(b2))
        .flatMap(_.addRook(r2))

      assertTrue(
        !snap.isBishop(1, 1), // nothing there
        !snap.isBishop(r1),   // rook there, not bishop
        snap.isBishop(b1),    // it is there
        snap.isBishop(b2),    // and here too
        !snap.isBishop(r2)    // but not here
      )

    },
    test("isVoid indicates that field is not occupied by any piece") {

      val (r1, b1, b2, r2) = ((3, 5), (7, 7), (5, 2), (4, 6))

      val Right(snap) = GameSnap.empty
        .addRook(r1)
        .flatMap(_.addBishop(b1))
        .flatMap(_.addBishop(b2))
        .flatMap(_.addRook(r2))

      assertTrue(
        snap.isVoid(7, 2),
        snap.isVoid(0, 5),
        !snap.isVoid(r1),
        !snap.isVoid(b1),
        !snap.isVoid(r2),
        !snap.isVoid(b2)
      )

    },
    suite("moveRook")(
      test("moves rook from a field containing a rook to some empty field") {

        val (b1, b2, r1, b3) = ((7, 7), (5, 2), (4, 6), (4, 1))
        val rCol             = (4, 2)
        val rRow             = (2, 6)

        val Right(snap) = GameSnap.empty
          .addRook(3, 5)
          .flatMap(_.addBishop(b1))
          .flatMap(_.addBishop(b2))
          .flatMap(_.addRook(r1))
          .flatMap(_.addBishop(b3))

        val colChecks = assertTrue(
          snap.isRook(r1),
          snap.isVoid(rCol),
          snap.moveRook(r1, rCol).map(_.isRook(r1)).contains(false),
          snap.moveRook(r1, rCol).map(_.isRook(rCol)).contains(true)
        )

        val rowChecks = assertTrue(
          snap.isRook(r1),
          snap.isVoid(rRow),
          snap.moveRook(r1, rRow).map(_.isRook(r1)).contains(false),
          snap.moveRook(r1, rRow).map(_.isRook(rRow)).contains(true)
        )

        val voidChecks = assertTrue {
          snap.moveRook((0, 0), (0, 1)) == Left(ROOK_NOT_THERE_MSG) && // can't move rook from empty field
          snap.moveRook(b1, (7, 6)) == Left(ROOK_NOT_THERE_MSG) &&     // can't move rook from field with bishop
          snap.moveRook(r1, b3) == Left(FIELD_NOT_VOID_MSG)            // can't move rook on non-empty field
        }

        colChecks && rowChecks && voidChecks

      },
      test("rook moves along row or column") {

        val r           = (3, 5)
        val Right(snap) = GameSnap.empty.addRook(r)

        assertTrue(
          snap.moveRook(r, (6, 5)).isRight &&                           // row move
            snap.moveRook(r, (3, 2)).isRight &&                         // col move
            snap.moveRook(r, (4, 6)) == Left(ROOK_CANNOT_MOVE_THERE) && // diagonal move is invalid
            snap.moveRook(r, (1, 2)) == Left(ROOK_CANNOT_MOVE_THERE)    // non row/col move is invalid
        )

      },
      test("rook cannot move through occupied fields") {

        val (r1, b1, r2) = ((3, 5), (3, 2), (5, 5))

        val Right(snap) = GameSnap.empty
          .addRook(r1)
          .flatMap(_.addBishop(b1))
          .flatMap(_.addRook(r2))

        assertTrue(
          snap.moveRook(r1, (3, 1)) == Left(ROOK_CANNOT_MOVE_THERE), // blocked by bishop
          snap.moveRook(r1, (3, 3)).isRight,                         // ok, stopped just before bishop
          snap.moveRook(r1, (6, 5)) == Left(ROOK_CANNOT_MOVE_THERE), // blocked by rook
          snap.moveRook(r1, (4, 5)).isRight                          // ok, stopped just before rook
        )
      }
    ),
    suite("moveBishop")(
      test("moves bishop from a field containing a bishop to some empty field") {

        val (b1, r1, r2, b2, r3) = ((6, 5), (7, 7), (5, 2), (4, 6), (5, 7))

        val Right(snap0) = GameSnap.empty
          .addBishop(b1)
          .flatMap(_.addRook(r1))
          .flatMap(_.addRook(r2))
          .flatMap(_.addBishop(b2))
          .flatMap(_.addRook(r3))

        val b2a = (1, 3)

        assertTrue(
          snap0.isBishop(b1),
          snap0.isBishop(b2),
          snap0.isVoid(b2a),
          snap0.moveBishop(b2, b2a).map(_.isBishop(b2)).contains(false),
          snap0.moveBishop(b2, b2a).map(_.isBishop(b2a)).contains(true)
        )

        val b2b = (3, 7)

        assertTrue(
          snap0.isBishop(b1),
          snap0.isVoid(b2b),
          snap0.moveBishop(b2, b2b).map(_.isBishop(b2)).contains(false),
          snap0.moveBishop(b2, b2b).map(_.isBishop(b2b)).contains(true)
        )

        assertTrue(
//          snap0.moveBishop((0, 0), (1, 1)) == Left(BISHOP_NOT_THERE_MSG) && // can't move bishop from empty field
          snap0.moveBishop(r1, (6, 6)) == Left(BISHOP_NOT_THERE_MSG) && // can't move bishop from field with rook
            snap0.moveBishop(b1, r3) == Left(FIELD_NOT_VOID_MSG)        // can't move bishop on non-empty field
        )

      },
      test("bishop moves along // or \\ diagonals") {

        val Right(snap0) = GameSnap.empty.addBishop(3, 5)

        assertTrue(snap0.moveBishop((3, 5), (1, 3)).isRight) // move up-left
        assertTrue(snap0.moveBishop((3, 5), (5, 3)).isRight) // move up-right

        assertTrue(snap0.moveBishop((3, 5), (1, 7)).isRight) // move down-left
        assertTrue(snap0.moveBishop((3, 5), (5, 7)).isRight) // move down-right

        assertTrue(snap0.moveBishop((3, 5), (5, 5)) == Left(BISHOP_CANNOT_MOVE_THERE)) // horizontal move is invalid
        assertTrue(snap0.moveBishop((3, 5), (3, 2)) == Left(BISHOP_CANNOT_MOVE_THERE)) // vertical move is invalid

      },
      test("bishop cannot move through occupied fields") {

        val Right(snap) = GameSnap.empty
          .addRook(6, 7)
          .flatMap(_.addBishop(3, 4))
          .flatMap(_.addBishop(0, 7))

        assertTrue(snap.moveBishop((3, 4), (6, 7)) == Left(FIELD_NOT_VOID_MSG)) // blocked by rook
        assertTrue(snap.moveBishop((3, 4), (5, 6)).isRight)                     // ok, stopped just before rook

        assertTrue(snap.moveBishop((3, 4), (0, 7)) == Left(FIELD_NOT_VOID_MSG)) // blocked by bishop
        assertTrue(snap.moveBishop((3, 4), (1, 6)).isRight)                     // ok, stopped just before bishop

      }
    ),
    suite("rectLine returns set of points on horizontal or vertical line between two locations")(
      test("find nothing if points are not on same row or col") {

        val notSameRowOrCol = fromBitStr("""
          | 0 0 1 0 0 0 0 0
          | 0 1 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)
        assertTrue(rectLine(notSameRowOrCol.get) == Nil)

      },
      test("find horizontal line as list of field indexes") {

        val onSameRow = fromBitStr("""
          | 0 0 0 0 0 0 0 0
          | 0 1 0 0 0 1 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)
        assertTrue(rectLine(onSameRow.get) == List(9, 10, 11, 12, 13))

      },
      test("find full row horizontal line as list of field indexes") {

        val onEdgesOfSameRow = fromBitStr("""
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 1 0 0 0 0 0 0 1
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)
        assertTrue(rectLine(onEdgesOfSameRow.get) == List(40, 41, 42, 43, 44, 45, 46, 47))
      },
      test("find vertical line as list of field indexes") {

        val onVerticalLine = fromBitStr("""
          | 0 0 0 0 0 0 0 0
          | 0 1 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 1 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)
        assertTrue(rectLine(onVerticalLine.get) == List(9, 17, 25, 33, 41))

      },
      test("find full column vertical line as list of field indexes") {

        val onEdgesOfSameColumn = fromBitStr("""
          | 0 0 0 1 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 1 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)
        assertTrue(rectLine(onEdgesOfSameColumn.get) == List(3, 11, 19, 27, 35, 43, 51, 59))

      },
      test("only find line if there's exactly 2 points") {

        val threePoints = fromBitStr("""
          | 0 0 0 0 1 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 1 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 1 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)

        val onePoint = fromBitStr("""
          | 0 0 0 0 0 0 0 1
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)

        assertTrue(
          rectLine(threePoints.get) == Nil,
          rectLine(onePoint.get) == Nil
        )

      }
    ),
    suite("diagLine returns set of points on diagonal line between two locations")(
      test("find nothing if points are not on same diagonal") {

        val notSameDiagonal = fromBitStr("""
          | 0 0 0 0 0 0 0 0
          | 0 1 1 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)
        assertTrue(diagLine(notSameDiagonal.get) == Nil)

      },
      test("find \\-sloped line as list of field indexes") {

        val onDiagonal = fromBitStr("""
          | 0 0 1 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 1 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)
        assertTrue(diagLine(onDiagonal.get) == List(2, 11, 20, 29))

      },
      test("find \\ diagonal spanning edge-to-edge as list of field indexes") {

        val edgeToEdge = fromBitStr("""
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 1 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 1 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)
        assertTrue(diagLine(edgeToEdge.get) == List(24, 33, 42, 51, 60))

      },
      test("find the longest \\ diagonal as list of field indexes") {

        val cornerToCorner = fromBitStr("""
          | 1 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 1
        """.stripMargin.filterNot(_.isWhitespace).reverse)
        assertTrue(diagLine(cornerToCorner.get) == List(0, 9, 18, 27, 36, 45, 54, 63))

      },
      test("find //-sloped diagonal as list of field indexes") {

        val onDiagonal = fromBitStr("""
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 1 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 1 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)
        assertTrue(diagLine(onDiagonal.get) == List(20, 27, 34, 41))

      },
      test("find // diagonal spanning edge-to-edge as list of field indexes") {

        val edgeToEdge = fromBitStr("""
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 1
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 1 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)
        assertTrue(diagLine(edgeToEdge.get) == List(31, 38, 45, 52, 59))

      },
      test("find the longest // diagonal as list of field indexes") {

        val cornerToCorner = fromBitStr("""
          | 0 0 0 0 0 0 0 1
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 1 0 0 0 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)
        assertTrue(diagLine(cornerToCorner.get) == List(7, 14, 21, 28, 35, 42, 49, 56))

      },
      test("only find line if there's exactly 2 points") {

        val threePoints = fromBitStr("""
          | 0 1 0 0 0 0 0 0
          | 0 0 1 0 0 0 0 0
          | 0 0 0 1 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)

        val onePoint = fromBitStr("""
          | 0 0 0 0 0 0 0 0
          | 0 1 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
          | 0 0 0 0 0 0 0 0
        """.stripMargin.filterNot(_.isWhitespace).reverse)

        assertTrue(
          diagLine(threePoints.get) == Nil,
          diagLine(onePoint.get) == Nil
        )
      }
    )
  )
}
