package domain

import domain.GameSnap.Error._

sealed trait SnapError

case class FailedToFetchSnaps(message: String) extends SnapError
case object CorruptSnap$ extends SnapError {
  val message: String = CORRUPT_SNAP_MSG
}
case object InvalidSnapHex extends SnapError {
  val message: String = INVALID_SNAP_HEX_MSG
}
