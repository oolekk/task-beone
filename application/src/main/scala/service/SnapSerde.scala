package service

import domain.GameSnap
import zio.ZIO
import zio.json._
import zio.kafka.serde.Serde
object SnapSerde {

  val snapSerde: Serde[Any, GameSnap] = Serde.string.inmapM(string =>
    ZIO
      .fromEither(string.fromJson[GameSnap])
      .mapError(msg => new RuntimeException(msg))
  )(gameSnap => ZIO.from(gameSnap.toJson))
}
