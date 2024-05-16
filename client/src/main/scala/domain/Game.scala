package domain

import util.BoardRender
import util.LongBitOps._

import scala.util.Random.nextLong

case class Game(
  id: String,
  round: Int = 0,
  saved: Int = 0,
  snaps: List[GameSnap] = Nil,
  logs: GameLog = GameLog.empty,
  pending: List[BoardCmd] = Nil
)

object Game {

  def init: Game = Game(nextLong.hexString)

  def rand: Game = {
    val snap = GameSnap.gen()
    val logs = GameLog.fresh(snap)
    Game(nextLong.hexString, 0, 0, List(snap), logs, Nil)
  }

  implicit class GameOps(game: Game) {
    def nextId: Int      = game.logs.on.headOption.map(_.id + 1).getOrElse(1)
    def snap: GameSnap   = game.snaps.headOption.getOrElse(GameSnap.empty)
    def noopInfo: String = s"$gameRound, last saved ${game.round - game.saved} rounds ago \n$boardInfo"
    def loadInfo: String = s"LOAD $noopInfo"
    def saveInfo: String = s"SAVE $noopInfo"

    def descAllInfo: String =
      game.logs.describeAll(game.round).mkString("\n")

    def descOneInfo(id: Int): String =
      game.logs.describeOne(id, game.round).getOrElse("NO SUCH ID")

    private def boardInfo: String = BoardRender.asGameBoard(game.snap)

    private def gameRound: String = s"GameId: ${game.id} round: ${game.round}"
  }

}
