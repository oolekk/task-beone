package util

import zio.test._

object LongBitOpsSpec extends ZIOSpecDefault {

  import util.LongBitOps._

  private val genIdx = Gen.int.map(v => (v % 64).abs)
  private def genIdxs(count: Int): Gen[Any, List[Int]] =
    Gen.listOfN(count)(genIdx)

  def spec: Spec[Any, Nothing] = suite("LongBitUtilTest")(
    test("for any long value fromString reconstitutes same long from bitString") {
      check(Gen.long) { long =>
        val bitStr = long.bitString
        assertTrue(
          fromString(bitStr).contains(long),
          fromString("0" * 64).contains(0L),
          fromString("1" * 64).contains(-1L)
        )
      }
    },
    test("fromString of all bits zero is zero") {
      assertTrue(LongBitOps.fromString("0" * 64).contains(0L))
    },
    test("fromString of only first bit pos is one") {
      assertTrue(LongBitOps.fromString("0" * 63 + "1").contains(1L))
    },
    test("fromString of all bits pos is minus one") {
      assertTrue(LongBitOps.fromString("1" * 64).contains(-1L))
    },
    test("setBitTo1 sets bit by index") {
      check(genIdx) { idx =>
        val long = 0L.setBitTo1(idx) // one only at idx
        assertTrue(long.getBit(idx) == 1)
      }
    },
    test("setBitTo0 sets bit by index") {
      check(genIdx) { idx =>
        val long = (-1L).setBitTo0(idx) // zero only at idx
        assertTrue(long.getBit(idx) == 0)
      }
    },
    test("find1 finds indexes of first 1-bit") {
      check(genIdx, genIdx, genIdx) { (x, y, z) =>
        val long  = 0L.setBitTo1(x).setBitTo1(y).setBitTo1(z)
        val first = x.min(y).min(z)
        assertTrue(long.find1.contains(first))
      }
    },
    test("find1s finds indexes of 1-bits, sorted, count1s counts them") {
      check(genIdxs(6)) { idxs =>
        val upToSixOneBits = idxs.foldLeft(0L)((l, i) => l.setBitTo1(i))
        assertTrue(
          upToSixOneBits.find1s == idxs.distinct.sorted,
          upToSixOneBits.count1s == idxs.distinct.length
        )
      }
    },
    test("find0 finds indexes of first 0-bit") {
      check(genIdx, genIdx, genIdx) { (x, y, z) =>
        val long  = (-1L).setBitTo0(x).setBitTo0(y).setBitTo0(z)
        val first = x.min(y).min(z)
        assertTrue(long.find0.contains(first))
      }
    },
    test("find0s finds indexes of 0-bits, sorted, count0s counts them") {
      check(genIdxs(9)) { idxs =>
        val upToNineZeroBits = idxs.foldLeft(-1L)((l, i) => l.setBitTo0(i))
        assertTrue(
          upToNineZeroBits.find0s == idxs.distinct.sorted,
          upToNineZeroBits.count0s == idxs.distinct.length
        )
      }
    },
    test("getBit retrieves bit by index as 0 or 1 int") {
      val bits = (1 to 64).map(_ => scala.util.Random.nextInt(2))
      val str  = bits.mkString.reverse
      val long = LongBitOps.fromString(str).get
      assertTrue(
        bits.zipWithIndex.forall { case (bit, index) => bit == long.getBit(index) }
      )
    },
    test("getBool retrieves bit by index as true or false") {
      val bools = (1 to 64).map(_ => scala.util.Random.nextBoolean())
      val str   = bools.map(if (_) "1" else "0").mkString.reverse
      val long  = LongBitOps.fromString(str).get
      assertTrue(
        bools.zipWithIndex.forall { case (bool, index) => bool == long.getBool(index) }
      )
    },
    test("bitString collects sign-bit first followed by most-to-least significant bit into string from left-to-right") {

      assertTrue(
        0L.bitString == ("0" * 64),
        1L.bitString == ("0" * 63) + 1,
        2L.bitString == ("0" * 62) + "10",
        3L.bitString == ("0" * 62) + "11",
        (Long.MaxValue - 1).bitString == "0" + ("1" * 62) + "0",
        Long.MaxValue.bitString == "0" + ("1" * 63),
        // bitwise, negative numbers are greater than positive numbers
        // due to the sign bit, but follow the arithmetic order otherwise
        Long.MinValue.bitString == "1" + ("0" * 63),
        (Long.MinValue + 1).bitString == "1" + ("0" * 62) + "1",
        (-3L).bitString == ("1" * 62) + "01",
        (-2L).bitString == ("1" * 62) + "10",
        (-1L).bitString == "1" * 64
      )

    },
    test("bitString lexicographic order follows the bitwise order") {
      assertTrue(
        // negative numbers are greater than positive numbers bitwise
        // due to the sign bit, but follow the arithmetic order otherwise
        (-1L).bitString > (-2L).bitString,
        (Long.MinValue + 1).bitString > Long.MinValue.bitString,
        Long.MinValue.bitString > Long.MaxValue.bitString,
        // positive numbers are smaller than negative numbers bitwise
        // due to the sign bit, but follow the arithmetic order otherwise
        Long.MaxValue.bitString > (Long.MaxValue - 1).bitString,
        2L.bitString > 1L.bitString,
        1L.bitString > 0L.bitString
      )
    }
  )

}
