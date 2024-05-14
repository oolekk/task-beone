package domain

import util.GameSnapUtil
import util.LongBitOps._

import scala.util.Random.nextLong

case class Game(id: String, round: Int = 0, saved: Int = 0, snaps: List[GameSnap] = Nil, log: GameLog = GameLog.empty)

object Game {

  val empty: Game = Game("")

  def empty(id: String): Game = Game(id)

  def init: Game = Game.empty(nextLong.hexString)

  def rand: Game = Game.init.copy(snaps = List(GameSnap.gen()))

  implicit class GameOps(game: Game) {
    def snap: GameSnap   = game.snaps.headOption.getOrElse(GameSnap.empty)
    def noopInfo: String = s"$gameRound last saved ${game.round - game.saved} rounds ago \n$boardInfo"
    def loadInfo: String = s"LOAD $noopInfo"
    def saveInfo: String = s"SAVE $noopInfo"

    def boardInfo: String = GameSnapUtil.asGameBoard(game.snap)

    def updateLog: Either[String, Game] =
      GameLog.replay(game).map(log => game.copy(log = log))

    def descAllInfo: String =
      game.log.describeAll(game.round).mkString("\n")

    def descOneInfo(id: Int): String =
      game.log.describeOne(id, game.round).getOrElse("NO SUCH ID")

    private def gameRound: String = s"GameId: ${game.id} round: ${game.round}"
  }

}
