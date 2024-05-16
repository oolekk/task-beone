package util

import domain.GameSnap

import scala.annotation.tailrec

object BoardRender {

  import util.LongBitOps._

  private val SIZE: Int = 8

  private val ROOK_CHAR   = '■'
  private val BISHOP_CHAR = '●'
  private val VOID_CHAR   = '·'

  def asGameBoard(
    snap: GameSnap,
    rook: Char = ROOK_CHAR,
    bishop: Char = BISHOP_CHAR,
    void: Char = VOID_CHAR
  ): String = {
    asMultiLine(snap, rook, bishop, void)
      .split('\n')
      .zipWithIndex
      .map { case (s, i) => i + "  " + s }
      .toList
      .mkString((0 to 7).mkString("   ", "  ", "\n"), "\n", "")
  }

  private def asMultiLine(
    snap: GameSnap,
    rook: Char = ROOK_CHAR,
    bishop: Char = BISHOP_CHAR,
    void: Char = VOID_CHAR
  ): String =
    asOneLine(snap, rook, bishop, void).sliding(SIZE, SIZE).map(_.mkString("  ")).mkString("\n")

  private def asOneLine(
    snap: GameSnap,
    rook: Char = ROOK_CHAR,
    bishop: Char = BISHOP_CHAR,
    void: Char = VOID_CHAR
  ): String =
    (0 until LONG_BIT_COUNT).map { i =>
      if (snap.bishops.getBool(i)) bishop
      else if (snap.rooks.getBool(i)) rook
      else void
    }.mkString

  def parse(
    input: String,
    skipDigits: Boolean = false,
    rook: Char = ROOK_CHAR,
    bishop: Char = BISHOP_CHAR
  ): GameSnap = {
    val cleanInput = input.filterNot(v => v.isWhitespace && (!skipDigits || v.isDigit))
    val safeLength = cleanInput.length.min(LONG_BIT_COUNT)
    @tailrec
    def loop(rs: Long, bs: Long, i: Int): GameSnap =
      if (i < safeLength) {
        if (cleanInput(i) == rook) loop(rs | (1L << i), bs, i + 1)
        else if (cleanInput(i) == bishop) loop(rs, bs | (1L << i), i + 1)
        else loop(rs, bs, i + 1)
      } else GameSnap(rs, bs)
    loop(0, 0, 0)
  }

}
