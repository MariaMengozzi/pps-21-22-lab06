package u06lab.solution

import u06lab.code.ConnectThree

import java.util.OptionalInt

object ConnectThree extends App:
  val bound = 3
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
    val elem = board.filter( d => d.x == x && d.y == y)
    if elem.isEmpty then None else Some(elem.head.player)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    val zip = board.filter(d => d.x == x)
    if zip.size - 1 == bound then None else Some(zip.size)

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    List.range(0, bound +1).foldLeft(Seq(): Seq[Board])((s, x) =>
      val y = firstAvailableRow(board, x)
      if y != None then (board :+ Disk(x, y.get, player)) +: s else s)

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(Seq(Seq()))
    case _ =>
      for
        game <- computeAnyGame(if player == X then O else X, moves - 1)
        board <- placeAnyDisk(game.head, player)
      yield
          board +: game


  def computeAnyGame2(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(Seq(Seq()))
    case _ =>
      for
        game <- computeAnyGame2(if player == X then O else X, moves - 1)
        board <- placeAnyDisk(game.head, player)
      yield
        if (win(game.head))
          game.distinct
        else
          (board +: game).distinct


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
        out = out || f.filter(d => d.x >= f.map(di => di.x).min && d.x < f.map(di => di.x).min + bound).size == bound
    out

  def checkCol(board: Board, player: Player): Boolean =
    var out = false
    for
      x <- 0 to bound
      f = board.filter(d => d.player == player && d.x == x)
      if !f.isEmpty
    do
      out = out || f.filter(d => d.y >= f.map(di => di.y).min && d.y < f.map(di => di.y).min + bound).size == bound
    out

  def checkDiag(board: Board, player: Player): Boolean =
    def ascending(board: Board, player: Player): Boolean =
      //ascending -> /
      var count = 0
      var out = false
      for
        x <- 0 to bound - (bound-1)
        y <- 0 to bound - (bound-1)
      do
        count = 0
        for
          c <- 0 until bound
          p = find(board, x+c, y+c)
        do
          if !p.isEmpty && p.get == player then
            count = count + 1
          out = out || count == bound
      out

    def descending(board: Board, player: Player): Boolean =
      //descending -> \
      var count = 0
      var out = false
      for
        x <- bound to bound - (bound-1) by -1
        y <- 0 to bound - (bound-1)
      do
        count = 0
        for
          c <- 0 until bound
        do
          val p = find(board, x-c, y+c)
          if !p.isEmpty && p.get == player then
            //println((x-c) + " " + (y+c))
            count = count + 1
          out = out || count == bound
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

/*  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O*/
// esempio di Board
/*  println("esempio di Board")
  printBoards(Seq(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X))))
  println()*/
/* println("EX 3: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 4).foreach { g =>
    printBoards(g)
    println()
  }*/
//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!

  val wr1 = List(Disk(0, 0, X), Disk(1, 0, X), Disk(2, 0, X), Disk(0, 3, O)) //win x
  val wr2 = List(Disk(0, 0, O), Disk(1, 0, X), Disk(2, 0, X), Disk(3, 0, O), Disk(0, 1, O), Disk(1, 1, X), Disk(2, 1, X), Disk(0, 2, X), Disk(3,1,X)) //win x
  val wr3 = List(Disk(0, 0, O), Disk(1, 0, O), Disk(2, 0, O), Disk(3, 0, O), Disk(0, 1, O), Disk(1, 1, X), Disk(2, 1, X), Disk(0, 2, X), Disk(3,1,X)) //win x
  val wr4_O = List(Disk(1,0,O), Disk(2,0,O), Disk(3,0,O), Disk(1,1,X), Disk(3,1,X))

  val lr1 = List(Disk(0, 0, O), Disk(1, 0, X), Disk(2, 0, X), Disk(0, 3, O)) //lose x
  val lr2 = List(Disk(0, 0, O), Disk(1, 0, X), Disk(2, 0, X), Disk(3, 0, O), Disk(0, 1, O), Disk(1, 1, X), Disk(2, 1, X), Disk(0, 2, X)) //lose x

  val wc1 = List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, O)) //win x
  val wc2 = List(Disk(0, 0, O), Disk(1, 0, O), Disk(1, 1, X), Disk(1, 2, X), Disk(1, 3, X), Disk(3, 0, X)) //win x

  val lc1 = List(Disk(0, 0, O), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, O)) //lose x
  val lc2 = List(Disk(0, 0, O), Disk(1, 0, O), Disk(1, 1, X), Disk(1, 2, X), Disk(1, 3, O), Disk(3, 0, X)) //lose x

  val wda = List(Disk(0, 0, X), Disk(1, 0 , O), Disk (2, 0, O), Disk(0, 1, O), Disk(1, 1, X), Disk(2, 1, X), Disk(0, 2, X), Disk(1, 2, O), Disk(2, 2, X))
  val wdd = List(Disk(0, 0, X), Disk(1, 0 , O), Disk (2, 0, O), Disk(3, 0, X), Disk(0, 1, O), Disk(1, 1, X), Disk(2, 1, X), Disk(0, 2, O), Disk(1, 2, X))
  val wd2 = List(Disk(1, 1, X), Disk(2, 2, X), Disk(3, 3, X), Disk(1, 0 ,O), Disk(2, 0, X))
/*
  printBoards(Seq(wd2))
  println("test win on row")
  // test row Win
  println (checkRow(wr1, X) || checkRow(wr2, X) || checkRow(wr3, X)) //true
  println(checkRow(wr4_O, O)) //true
  // test row no Win
  println(checkRow(lr1, X) || checkRow(lr2, X)) //false

  println("test win on col")
  // test row Win
  println (checkCol(wc1, X) || checkCol(wc2, X)) //true
  // test row no Win
  println (checkCol(lc1, X) || checkCol(lc2, X)) //false

  println("test win row no cols")
  println(checkRow(wr1, X) || checkCol(wr1, X)) //true
  println("test win cols no row")
  println(checkRow(wc1, X) || checkCol(wc1, X)) //true

  println("test win")
  println(win(wr4_O)) //true

  println("test lose")
  println(win(lc1)) //false

  println("test diagonal -> /")
  println(checkDiag(wda, X))
  println(checkDiag(wdd, X))
  println(checkDiag(wd2, X))
  println(checkDiag(wr1, X))

  println("test diagonal -> \\")
  println(checkDiag(wda, X))
  println(checkDiag(wdd, X))
  println(checkDiag(wd2, X))
  println(checkDiag(wr1, X))

  val l = List(Disk(0,0,O), Disk(1,0,X), Disk(0,1,X), Disk(1,1,O), Disk(0,2,O), Disk(1,2,X))
  /*printBoards(Seq(l))
  println(checkRow(l, X))
  println(checkRow(l, O))

  println(checkCol(l, X))
  println(checkCol(l, O))*/

  println(checkDiag(l, X))
  println(checkDiag(l, O))

  println("Test 3 check")
  println(win(wr1)) // true
  println(win(wc1)) //true
  println(win(wda)) //true

  println(win(lc2)) // false

  val l2 = List(Disk(0,0,X), Disk(0,0,X), Disk(1,0,O), Disk(2,0,O), Disk(2,0,X))
  println(win(l2))
  */

  computeAnyGame2(O, 7).foreach { g =>
    printBoards(g)
    println()
  }
