package domain

case class Game(id: String, round: Int, moved: GameSnap, taken: GameSnap)

object Game {

  // immutable, can be re-used safely
  val empty: Game             = Game("", 0, GameSnap.empty, GameSnap.empty)
  def empty(id: String): Game = Game(id, 0, GameSnap.empty, GameSnap.empty)
}
