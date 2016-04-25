package game

object Dir {
  sealed trait Direction
  case object North extends Direction
  case object West extends Direction
  case object East extends Direction
  case object South extends Direction

  private[this] val dirs = Seq(North, East, South, West)
  // clock-wise
  private[this] val first = dirs.head
  private[this] val last = dirs.last

  def succ(p: Direction): Direction = dirs.dropWhile(_ != p).tail.headOption.getOrElse(first)
  def pred(p: Direction): Direction = dirs.takeWhile(_ != p).lastOption.getOrElse(last)
}

case class Point(x: Int, y: Int) {
  /**
    * Return next point coordinates for specified direction
    *
    * @param d step direction
    * @return
    */
  def step(d: Dir.Direction): Point = d match {
    case Dir.North => Point(x, y + 1)
    case Dir.South => Point(x, y - 1)
    case Dir.East => Point(x + 1, y)
    case Dir.West => Point(x - 1, y)
  }
}

trait Board {
  def isMarked(point: Point): Boolean

  def mark(point: Point, color: Option[java.awt.Color])
}

/**
  * Ant prototype
  *
  * point - current point on board
  * dir - current direction
  * color - color for marking point
  * move - do one step with following rules:
  * - current point NOT marked - turn 90 left, mark point and make step
  * - current point Marked - turn 90 right, UNmark point and make step
  */
trait Ant {
  def point: Point

  def dir: Dir.Direction

  def color: java.awt.Color

  def move(board: Board): Ant
}

/**
  * Immutable Ant
  *
  * @param point point on board
  * @param color sign of the ant
  */
case class ScalaAnt(point: Point, dir: Dir.Direction, color: java.awt.Color) extends Ant {
  /**
    * move - do one step with following rules:
    * - current point NOT marked - turn 90 left, mark point and make step
    * - current point Marked - turn 90 right, UNmark point and make step
    * @param board board for move
    * @return new Ant incarnation
    */
  override def move(board: Board): Ant = {
    if (board.isMarked(point)) {
      board.mark(point, None)
      val d = Dir.succ(dir)
      ScalaAnt(point.step(d), d, color)
    } else {
      board.mark(point, Some(color))
      val d = Dir.pred(dir)
      ScalaAnt(point.step(d), d, color)
    }
  }
}

