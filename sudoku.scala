sealed trait Cell
case class KnownCell(value: Int) extends Cell {
  override def toString = value.toString
}
case object UnknownCell extends Cell {
  override def toString = " "
}

object Sudoku extends App {
  val cols = ('1' to '9').map(_.toString)
  val rows = ('A' to 'I').map(_.toString)

  case class Pos(row: String, col: String) {
    def siblings = {
      val rowSiblings = rows.grouped(3).find(_.contains(row)).get
      val colSiblings = cols.grouped(3).find(_.contains(col)).get
      val blockSiblings = cross(rowSiblings, colSiblings).map(Pos.tupled)
  
      (cols.map(Pos(row, _)) ++ rows.map(Pos(_, col)) ++ blockSiblings).toSet
    }
  }

  def knownCells(g: Map[Pos, Cell]): Int = g.count(_._2.isInstanceOf[KnownCell])

  def constrained(g: Map[Pos, Cell]): Map[Pos, Cell] = {
    val newGrid = g.map({ 
      case (p, KnownCell(v)) => p -> KnownCell(v)
      case (p, UnknownCell) => candidates(g, p) match {
        case Some(cs) if (cs.size == 1) =>  p -> KnownCell(cs.head)
        case _ =>                           p -> UnknownCell
      }
    }).toMap
   
    if (knownCells(g) == knownCells(newGrid)) g else constrained(newGrid)
  }

  def candidates(g: Map[Pos, Cell], pos: Pos): Option[Set[Int]] = {
    val used = pos.siblings.map(g.apply).collect({ case KnownCell(v) => v })
    val cs = (1 to 9).toSet -- used.toSet

    if (cs.size == 0) None else Some(cs)
  }

  def solve(rawGrid: Map[Pos, Cell]): Option[Map[Pos, Cell]] = {
    val grid = constrained(rawGrid)

    val unknowns = grid.collect({
      case (p, UnknownCell) => candidates(grid, p).map(p -> _)
    }).toSeq

    if (unknowns.isEmpty) {
      Some(grid)
    } else {
      sequence(unknowns).flatMap(cs => {
        cs.sortBy(_._2.size).flatMap({
          case (p, cs) => cs.map(p -> _)
        }).foldLeft(Option.empty[Map[Pos, Cell]])((acc, el) =>
          acc.orElse(solve(grid + (el._1 -> KnownCell(el._2))))
        )
      })
    }
  }

  def printGrid(grid: Map[Pos, Cell]): String = {
    rows.map(r =>
      cols.map(c =>
        grid(Pos(r, c)).toString
      ).grouped(3).map(_.mkString(" ")).mkString("|")
    ).grouped(3).map(_.mkString("\n")).mkString("\n" + "-" * 17 + "\n")
  }

  def parseGrid(grid: Seq[Seq[Cell]]): Map[Pos, Cell] = {
    rows.zip(grid.map(cols.zip(_)))
      .flatMap({ case (r, cs) => cs.map({ case (c, v) => Pos(r, c) -> v }) }).toMap
  }

  val g = parseGrid(Seq[Seq[Cell]](
    Seq(7, 9, ?, ?, ?, ?, 3, ?, ?),
    Seq(?, ?, ?, ?, ?, 6, 9, ?, ?),
    Seq(8, ?, ?, ?, 3, ?, ?, 7, 6),
    Seq(?, ?, ?, ?, ?, 5, ?, ?, 2),
    Seq(?, ?, 5, 4, 1, 8, 7, ?, ?),
    Seq(4, ?, ?, 7, ?, ?, ?, ?, ?),
    Seq(6, 1, ?, ?, 9, ?, ?, ?, 8),
    Seq(?, ?, 2, 3, ?, ?, ?, ?, ?),
    Seq(?, ?, 9, ?, ?, ?, ?, 5, 4)
  ))

  println(printGrid(g))
  println("\n======================\n")
  println(printGrid(solve(g).get))

  def cross[T](xs: Traversable[T], ys: Traversable[T]) = xs.flatMap(x => ys.map(x -> _))

  def sequence[T](l: Seq[Option[T]]): Option[Seq[T]] =
    l.foldRight(Option(List.empty[T]))((optInt, acc) =>
      for { tail <- acc; int <- optInt } yield int :: tail
    )

  implicit def int2cell(i: Int): Cell = KnownCell(i)
  def ? = UnknownCell
}
