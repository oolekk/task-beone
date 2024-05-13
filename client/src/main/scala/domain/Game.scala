package domain

case class Game(id: String, round: Int, snaps: List[GameSnap], savedAt: Int = 0)

object Game {

  // immutable, can be re-used safely
  val empty: Game             = Game("", 0, Nil)
  def empty(id: String): Game = Game(id, 0, Nil)

  implicit class GameOps(game : Game) {
    def snap: GameSnap = game.snaps.headOption.getOrElse(GameSnap.empty)
  }

}
