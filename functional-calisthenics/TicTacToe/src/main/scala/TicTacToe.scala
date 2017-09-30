object TicTacToe {

  /**
    * Rules
    * •    X always goes first.
    * •    Players alternate placing Xs and Os on the board until either:
    * o    One player has three in a row, horizontally, vertically or diagonally
    * o    All nine squares are filled.
    * •    If a player is able to draw three Xs or three Os in a row, that player wins.
    * •    If all nine squares are filled and neither player has three in a row, the game is a draw.
    */
  def main(args: Array[String]): Unit = {

  }

  sealed trait Player
  case object X extends Player
  case object O extends Player

  sealed trait Row
  case object Top extends Row
  case object Middle extends Row
  case object Bottom extends Row
  sealed trait Column
  case object Left extends Column
  case object Center extends Column
  case object Right extends Column
  type Cell = (Row, Column, Player)
  type Board = Stream[Row]
  type GameState = (Board, Player)

  def play
}
