package homework

import scala.util.Random

class BallsExperiment {

  sealed trait Ball
  case object BlackBall extends Ball
  case object WhiteBall extends Ball

  val ballsUrn: List[Ball] = List(WhiteBall, WhiteBall, WhiteBall, BlackBall, BlackBall, BlackBall)

  def checkBall(ballsUrn: List[Ball], ball: Ball): (Boolean, List[Ball]) = {
    val ballItem = Random.nextInt(ballsUrn.size)
    val check = if (ballsUrn(ballItem) == ball) true else false
    (check, ballsUrn.zipWithIndex.filter(_._2 != ballItem).map(_._1))
  }

  def isFirstBlackSecondWhite: Boolean = {
    val checkBlackBall: (Boolean, List[Ball]) = checkBall(ballsUrn, BlackBall)
    if (checkBlackBall._1) {
      val isWhite = checkBall(checkBlackBall._2, WhiteBall)
      isWhite._1
    } else checkBlackBall._1
  }
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 1000000
    val listOfExperiments: List[BallsExperiment] = List.fill(count)(new BallsExperiment)
    val countOfExperiments = listOfExperiments.map(_.isFirstBlackSecondWhite)
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}