package util

object LongBitOps {

  // Low level bit operations, 64-bits for 64 fields on board
  // Will be used to validate moves and board rapidly

  val LONG_BITS = 64
  def fromString(bits: String): Option[Long] = if (bits.length == LONG_BITS) {
    // bits must be 64 chars long, containing only '0's and '1's to return Some
    var i: Int = 0
    var long: Long = 0L
    // efficient single pass with early-finish drop-while trick
    Option.when(bits.dropWhile { // all should drop as we accumulate
      case '0' => i = i + 1; true // bit already zero, just continue
      // in string actually the last character is our first bit, therefore bit shift is LONG_BITS - i - 1
      case '1' => long = long | (1L << LONG_BITS - i - 1); i = i + 1; true // set bit to 1 and continue
      case _ => false // not a zero and not a 1, will finish with non-empty reminder, results in None
    }.isEmpty)(long)
  } else None

  implicit class LongBitImpl(bits: Long) {

    def getBit(i: Int): Long = (bits >>> i) & 1 // must be triple/unsigned shift

    def getBool(i: Int): Boolean = getBit(i) == 1

    def bitString: String = // first bit is the last char in string
      (1 to LONG_BITS).map(i => if (getBool(LONG_BITS - i)) '1' else '0').mkString

    def setBitTo0(i: Int): Long = bits & ~(1L << i) // right-hand is single 0

    def setBitTo1(i: Int): Long = bits | (1L << i) // right-hand is single 1

    def find1: Option[Int] =
      (0 until LONG_BITS).find(bits.getBool(_))
    def find1s: List[Int] =
      (0 until LONG_BITS).filter(bits.getBool(_)).toList
    def count1s: Int =
      (0 until LONG_BITS).count(bits.getBool(_))

    def find0: Option[Int] =
      (0 until LONG_BITS).find(!bits.getBool(_))
    def find0s: List[Int] =
      (0 until LONG_BITS).filter(!bits.getBool(_)).toList
    def count0s: Int =
      (0 until LONG_BITS).count(!bits.getBool(_))

  }

}
