import java.awt.Color

import game.Dir._
import game._
import org.scalatest.FunSuite

class GameSuite extends FunSuite {

  // (North, East, South, West)
  test("pred should correctly return Direction") {
    assert(turnLeft(North) == West)
    assert(turnLeft(West) == South)
    assert(turnLeft(South) == East)
    assert(turnLeft(East) == North)
  }

  test("succ should correctly return Direction") {
    assert(turnRight(North) == East)
    assert(turnRight(East) == South)
    assert(turnRight(South) == West)
    assert(turnRight(West) == North)
  }

  test("point should correctly change coordinates on step") {
    assert(Point(0, 0).step(North) == Point(0, 1))
    assert(Point(0, 0).step(South) == Point(0, -1))
    assert(Point(0, 0).step(East) == Point(1, 0))
    assert(Point(0, 0).step(West) == Point(-1, 0))
    assert(Point(0, 0).step(North).step(South) == Point(0, 0))
    assert(Point(0, 0).step(North).step(East) == Point(1, 1))
  }

  private case class FakeBoard(p: Point) extends Board {
    override def isMarked(point: Point): Boolean = point == p

    override def flip(p: Point, c: java.awt.Color): Unit = {}
  }

  // - current point Marked - turn 90 right, UNmark point and make step
  test("Scala Ant should correctly step on fake board from marked point") {
    val board = FakeBoard(Point(0, 0))
    val ant = ScalaAnt(Point(0, 0), North, java.awt.Color.MAGENTA).move(board)
    assert(ant.point == Point(1, 0))
    assert(ant.dir == East)
  }

  // - current point NOT marked - turn 90 left, mark point and make step
  test("Scala Ant should correctly step on fake board from UNmarked point") {
    val board = FakeBoard(Point(5, 5))
    val ant = ScalaAnt(Point(0, 0), North, java.awt.Color.MAGENTA).move(board)
    assert(ant.point == Point(-1, 0))
    assert(ant.dir == West)
  }

  // - current point Marked - turn 90 right, UNmark point and make step
  test("Monocle Ant should correctly step on fake board from marked point") {
    val board = FakeBoard(Point(0, 0))
    val ant = MonocleAnt(Point(0, 0), North, java.awt.Color.MAGENTA).move(board)
    assert(ant.point == Point(1, 0))
    assert(ant.dir == East)
  }

  // - current point NOT marked - turn 90 left, mark point and make step
  test("Monocle Ant should correctly step on fake board from UNmarked point") {
    val board = FakeBoard(Point(5, 5))
    val ant = MonocleAnt(Point(0, 0), North, java.awt.Color.MAGENTA).move(board)
    assert(ant.point == Point(-1, 0))
    assert(ant.dir == West)
  }

  test("Map board correctly mark and unmark points") {
    val p = Point(1, 2)
    val b = InfinityBoard()
    assert(!b.isMarked(p))
    b.flip(p, java.awt.Color.CYAN)
    assert(b.isMarked(p))
    b.flip(p, java.awt.Color.WHITE)
    assert(!b.isMarked(p))
  }

}