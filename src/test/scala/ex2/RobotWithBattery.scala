package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotWithBatterySpec extends AnyFlatSpec with Matchers:
  "A RobotWithBattery" should "act correctly if charged" in:
    val initialPosition = (0, 0)
    val robot = new RobotWithBattery(new SimpleRobot(initialPosition, Direction.North))
    robot.act()
    robot.position should be((0, 1))
    robot.act()
    robot.position should be((0, 2))
    robot.turn(Direction.West)
    robot.act()
    robot.position should be((-1, 2))
    robot.act()
    robot.position should be((-2, 2))

  it should "stop if there isn't enough charge" in:
    val initialPosition = (0, 0)
    val robot = new RobotWithBattery(new SimpleRobot(initialPosition, Direction.North))
    robot.act()
    robot.position should be((0, 1))
    robot.act()
    robot.position should be((0, 2))
    robot.turn(Direction.South)
    robot.act()
    robot.position should be((0, 1))
    robot.act()
    robot.position should be((0, 0))
    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 0))
    robot.act()
    robot.position should be((1, 0))