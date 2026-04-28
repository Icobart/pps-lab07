package ex2

import scala.annotation.tailrec
import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot) extends Robot:
  export robot.{act as _, *}
  private var battery = 100
  private val cost = 20
  override def act(): Unit = battery match
    case b if b >= cost =>
      battery -= cost
      robot.act()
    case _ => println("battery exhausted, current level: "+ battery)

class RobotCanFail(val robot: Robot, val failChance: Double) extends Robot:
  export robot.{act as _, *}
  private val random = new Random()
  override def act(): Unit =
    if random.nextDouble() > failChance then robot.act() else println("robot failed")

class RobotRepeated(val robot: Robot, val repetitions: Int) extends Robot:
  export robot.{act as _, *}
  override def act(): Unit =
    for i <- 0 until repetitions do
      robot.act()

@main def testRobot(): Unit =
  val initialPosition = (0, 0)
  val robot = LoggingRobot(SimpleRobot(initialPosition, Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
  println("\nRobotCanFail running...")
  val probability = 0.3
  val robotCanFail = RobotCanFail(SimpleRobot(initialPosition, Direction.North), probability)
  var previousPosition = initialPosition
  robotCanFail.act()
  @tailrec
  def runRobotCanFail(): Unit = robotCanFail.position match
    case p if p == previousPosition => println("Robot ended in: "+ p)
    case p =>
      previousPosition = p
      robotCanFail.act()
      runRobotCanFail()
  runRobotCanFail()
