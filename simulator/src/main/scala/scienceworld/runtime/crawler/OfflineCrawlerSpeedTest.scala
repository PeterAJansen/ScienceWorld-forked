package scienceworld.runtime.crawler

import scienceworld.runtime.crawler.OfflineCrawler.interface
import scienceworld.runtime.pythonapi.PythonInterface

import collection.JavaConverters._
import scala.util.Random


object OfflineCrawlerSpeedTest {


  // Run a list of actions
  val interface = new PythonInterface()

  def runRandomActionSequence(taskIdx:Int, varIdx:Int, numActionsToRun:Int, simplificationStr:String = "easy"):Long = {
    val startTime = System.currentTimeMillis()

    val taskName = interface.getTaskNames().asScala(taskIdx)
    interface.load(taskName, varIdx, simplificationStr)

    var description:String = ""
    var score:Double = 0.0
    var isCompleted:Boolean = false

    for (actionIdx <- 0 until numActionsToRun) {
      //val validActions = interface.getValidActionObjectCombinations().asScala.toArray
      //val randActionIdx = Random.nextInt(validActions.length)
      //val actionStr = validActions(randActionIdx)

      val actionStr = "look around"

      description = interface.step(actionStr)
      score = interface.getScore()
      isCompleted = interface.isComplete
    }

    val deltaTime = System.currentTimeMillis() - startTime
    println("Total execution time: " + deltaTime + " msec")

    return deltaTime
  }


  def main(args:Array[String]) = {
    val numEpisodes:Int = 100

    val taskIdx:Int = 0
    val varIdx:Int = 0
    val numActionsToRun:Int = 10

    var totalRuntime:Long = 0
    for (i <- 0 until numEpisodes) {
      totalRuntime += this.runRandomActionSequence(taskIdx, varIdx, numActionsToRun, simplificationStr = "easy")
    }


    val avgRuntime = totalRuntime.toDouble / numEpisodes.toDouble
    println ("Average runtime per episode: " + avgRuntime + " msec/episode")
    println ("Rate: " + 1/(avgRuntime*0.001) + " episodes/sec")

  }

}
