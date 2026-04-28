package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotRepeatedSpec extends AnyFlatSpec with Matchers:
  "A RobotRepeated" should "act 5 times correctly" in:
    val robot = new RobotRepeated(new SimpleRobot((0, 0), Direction.North), 5)
    robot.act()
    robot.position should be((0, 5))
    robot.act()
    robot.position should be((0, 10))

  it should "change direction and act correctly" in :
    val robot = new RobotRepeated(new SimpleRobot((0, 0), Direction.North), 3)
    robot.act()
    robot.position should be((0, 3))
    robot.turn(Direction.East)
    robot.act()
    robot.position should be((3, 3))

  it should "not move if repetitions is 0" in :
    val robot = new RobotRepeated(new SimpleRobot((0, 0), Direction.North), 0)
    robot.act()
    robot.position should be((0, 0))