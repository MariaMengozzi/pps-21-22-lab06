package u06lab.solution

object TicTacToe extends App:
  val bound = 2
  val tris = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.filter( d => d.x == x && d.y == y).map(d=> d.player).headOption

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    val f = board.filter(d => d.x == x)
    Option.when(f.size-1 < bound)(f.size)

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    List.range(0, bound +1).foldLeft(Seq(): Seq[Board])((s, x) =>
      val y = firstAvailableRow(board, x)
      if y != None then (board :+ Disk(x, y.get, player)) +: s else s)


  def computeAnyGame(player: Player, moves: Int): LazyList[Game] =
    def _computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
      case 0 => LazyList(Seq(Seq()))
      case _ =>
        for
          game <- _computeAnyGame(if player == X then O else X, moves - 1)
          board <- placeAnyDisk(game.head, player)
        yield
          if (win(game.head))
            game
          else
            board +: game
    _computeAnyGame(player, moves).distinct


  def win(board: Board): Boolean =
    var win = false
    for
      p <- List(X, O)
    do
      win = win || checkRow(board, p) || checkCol(board, p) || checkDiag(board, p)
    win


  def checkRow(board: Board, player: Player): Boolean =
    var out = false
    for
      y <- 0 to bound
      f = board.filter(d => d.player == player && d.y == y)
      if !f.isEmpty
    do
      out = out || f.filter(d => d.x >= f.map(di => di.x).min && d.x < f.map(di => di.x).min + tris).size == tris
    out

  def checkCol(board: Board, player: Player): Boolean =
    var out = false
    for
      x <- 0 to bound
      f = board.filter(d => d.player == player && d.x == x)
      if !f.isEmpty
    do
      out = out || f.filter(d => d.y >= f.map(di => di.y).min && d.y < f.map(di => di.y).min + tris).size == tris
    out

  def checkDiag(board: Board, player: Player): Boolean =
    def ascending(board: Board, player: Player): Boolean =
      //ascending -> /
      var count = 0
      var out = false
      for
        x <- 0 to bound - (tris-1)
        y <- 0 to bound - (tris-1)
      do
        count = 0
        for
          c <- 0 until tris
          p = find(board, x+c, y+c)
        do
          if !p.isEmpty && p.get == player then
            count = count + 1
          out = out || count == tris
      out

    def descending(board: Board, player: Player): Boolean =
      //descending -> \
      var count = 0
      var out = false
      for
        x <- bound to bound - (tris-1) by -1
        y <- 0 to bound - (tris-1)
      do
        count = 0
        for
          c <- 0 until tris
        do
          val p = find(board, x-c, y+c)
          if !p.isEmpty && p.get == player then
          //println((x-c) + " " + (y+c))
            count = count + 1
          out = out || count == tris
      out

    ascending(board, player) || descending(board, player)


  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()


  computeAnyGame(O, 6).foreach { g =>
    printBoards(g)
    println()
  }