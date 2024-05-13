package util

object LongBitOps {

  // Low level bit operations, 64-bits for 64 fields on board
  // Will be used to validate moves and board rapidly

  val LONG_BIT_COUNT = 64
  val LONG_HEX_COUNT = 16

  private val BIT_BIT_SIZE = 1
  private val HEX_BIT_SIZE = 4

  private val HEX_CHARS = "abcdef0123456789ABCDEF"

  def fromBitStr(bits: String): Option[Long] = if (bits.length == LONG_BIT_COUNT) {
    // bits must be 64 chars long, containing only '0's and '1's
    var long: Long = 0L
    var idx: Int   = LONG_BIT_COUNT - BIT_BIT_SIZE
    // efficient single pass with early-finish drop-while
    Option.when(bits.dropWhile { c =>
      if (c == '0') { idx = idx - BIT_BIT_SIZE; true } // bit already zero
      else {
        (c == '1') && {
          long = long | (1L << idx);
          idx = idx - BIT_BIT_SIZE; true
        }
      }
    }.isEmpty)(long)
  } else None

  def fromHexStr(hex: String): Option[Long] = if (hex.length == LONG_HEX_COUNT) {
    var long = 0L
    var idx  = LONG_BIT_COUNT - HEX_BIT_SIZE
    Option.when(hex.dropWhile { c =>
      if (c == '0') { idx = idx - HEX_BIT_SIZE; true } // hex bits already zero
      else {
        val at = HEX_CHARS.indexOf(c)
        (at >= 0) && {
          long = long | (((10L + at) % LONG_HEX_COUNT) << idx)
          idx = idx - HEX_BIT_SIZE;
          true
        }
      }
    }.isEmpty)(long)
  } else None

  implicit class LongBitImpl(bits: Long) {

    def xor(long: Long): Long = bits ^ long

    def not(long: Long): Long = bits & ~long

    def overlaps(long: Long): Boolean = (bits & long) != 0

    def getBit(i: Int): Long = (bits >>> i) & 1 // must be unsigned shift

    def getBool(i: Int): Boolean = getBit(i) == 1

    def bitString: String = {
      val s = bits.toBinaryString
      if (bits < 0) s
      else "0" * (64 - s.length) + s
    }

    def hexString: String = {
      val hex = bits.toHexString
      if (hex.length == 16) hex
      else "0" * (16 - hex.length) + hex
    }

    def setBitTo0(i: Int): Long = bits & ~(1L << i) // right-hand is single 0

    def setBitTo1(i: Int): Long = bits | (1L << i) // right-hand is single 1

    def find1: Option[Int] =
      (0 until LONG_BIT_COUNT).find(bits.getBool(_))
    def find1s: List[Int] =
      (0 until LONG_BIT_COUNT).filter(bits.getBool(_)).toList
    def count1s: Int =
      (0 until LONG_BIT_COUNT).count(bits.getBool(_))

    def find0: Option[Int] =
      (0 until LONG_BIT_COUNT).find(!bits.getBool(_))
    def find0s: List[Int] =
      (0 until LONG_BIT_COUNT).filter(!bits.getBool(_)).toList
    def count0s: Int =
      (0 until LONG_BIT_COUNT).count(!bits.getBool(_))

  }

}
