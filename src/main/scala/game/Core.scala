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

  private[this] def succ(p: Direction): Direction = dirs.dropWhile(_ != p).tail.headOption.getOrElse(first)
  private[this] def pred(p: Direction): Direction = dirs.takeWhile(_ != p).lastOption.getOrElse(last)

  def turnLeft(p: Direction): Direction = pred(p)
  def turnRight(p: Direction): Direction = succ(p)
}

case class Point(x: Int, y: Int)

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

trait Step {
  def step(p: Point, d: Dir.Direction): Point
}

trait LAStep extends Step {
  /**
    * Return next point coordinates for specified direction
    *
    * @param p ant current position
    * @param d step direction
    * @return
    */
  def step(p: Point, d: Dir.Direction): Point = d match {
    case Dir.North => Point(p.x, p.y + 1)
    case Dir.South => Point(p.x, p.y - 1)
    case Dir.East => Point(p.x + 1, p.y)
    case Dir.West => Point(p.x - 1, p.y)
  }
}

trait Board {
  def isMarked(p: Point): Boolean

  /**
    * Mark if not marked with color
    * UnMark if already marked
    *
    * @param p point
    * @param c color
    */
  def mark(p: Point, c: java.awt.Color)
}

case class MapBoard() extends Board {
  private[this] var m = Map.empty[Point, java.awt.Color]

  override def isMarked(p: Point): Boolean = m.contains(p)

  override def mark(p: Point, c: java.awt.Color): Unit = if (isMarked(p)) m -= p else m += p -> c
}

/**
  * Standard Ant
  *
  * @param point point on board
  * @param color sign of the ant
  */
case class ScalaAnt(point: Point, dir: Dir.Direction, color: java.awt.Color) extends Ant with LAStep {
  /**
    * move - do one step with following rules:
    * - current point NOT marked - turn 90 left, mark point and make step
    * - current point Marked - turn 90 right, UNmark point and make step
    *
    * @param board board for move
    * @return new Ant incarnation
    */
  override def move(board: Board): Ant = {
    val d = if (board.isMarked(point)) Dir.turnRight(dir) else Dir.turnLeft(dir)
    board.mark(this.point, this.color)
    val p = step(this.point, d)
    ScalaAnt(p, d, color)
  }
}

/**
  * Lens-powered Ant
  *
  * @param point point on board
  * @param color sign of the ant
  */
case class MonocleAnt(point: Point, dir: Dir.Direction, color: java.awt.Color) extends Ant with LAStep {
  private[this] val pointLens = monocle.Lens[MonocleAnt, Point](_.point)(v => a => a.copy(point = v))
  private[this] val dirLens = monocle.Lens[MonocleAnt, Dir.Direction](_.dir)(v => a => a.copy(dir = v))

  /**
    * move - do one step with following rules:
    * - current point NOT marked - turn 90 left, mark point and make step
    * - current point Marked - turn 90 right, UNmark point and make step
    *
    * @param board board for move
    * @return new Ant incarnation
    */
  override def move(board: Board): Ant = {
    val d = if (board.isMarked(point)) Dir.turnRight(dir) else Dir.turnLeft(dir)
    board.mark(this.point, this.color)
    dirLens.set(d)(pointLens.set(step(this.point, d))(this))
  }
}
