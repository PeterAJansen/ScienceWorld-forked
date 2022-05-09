package scienceworld.runtime.crawler

import main.scala.scienceworld.runtime.SimplifierProcessor
import scienceworld.runtime.pythonapi.PythonInterface

import scala.util.control.Breaks.{break, breakable}
import collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


object OfflineCrawler {
  val interface = new PythonInterface()

  // Generate gold action sequences
  def getGoldPath(taskIdx:Int, varIdx:Int, simplificationStr:String = "easy"):Array[String] = {
    //val interface = new PythonInterface()
    val taskName = interface.getTaskNames().asScala(taskIdx)
    interface.load(taskName, varIdx, simplificationStr, generateGoldPath = true)

    val goldActions = interface.getGoldActionSequence().asScala.toArray

    // Return
    return goldActions
  }


  // Run a list of actions
  def runActionSequence(taskIdx:Int, varIdx:Int, actionsToRun:Array[String], simplificationStr:String = "easy"):(String, Double, Boolean, Array[String]) = {
    //val interface = new PythonInterface()
    val taskName = interface.getTaskNames().asScala(taskIdx)
    interface.load(taskName, varIdx, simplificationStr)

    var description:String = ""
    var score:Double = 0.0
    var isCompleted:Boolean = false

    for (actionStr <- actionsToRun) {
      description = interface.step(actionStr)
      score = interface.getScore()
      isCompleted = interface.isComplete
    }

    val validActions = interface.getValidActionObjectCombinations().asScala.toArray


    // Return
    return (description, score, isCompleted, validActions)
  }


  def crawlToDepth(taskIdx:Int, varIdx:Int, actionsSoFar:Array[String], maxDepth:Int, allowableActionStarts:Set[String], simplificationStr:String = "easy"): Unit = {

    // Stop condition
    if (actionsSoFar.length > maxDepth) return

    // Get info for this node
    val (description, score, isCompleted, validActions) = runActionSequence(taskIdx, varIdx, actionsSoFar, simplificationStr)

    for (actionStr <- validActions) {
      val actionStart = actionStr.split(" ")(0)   // Only continue with this action if it's on the list of allowed actions
      if (allowableActionStarts.contains(actionStart)) {
        crawlToDepth(taskIdx, varIdx, actionsSoFar ++ Array(actionStr), maxDepth, allowableActionStarts, simplificationStr)
      }

    }


  }



  def main(args:Array[String]): Unit = {
    val taskNames = interface.getTaskNames().asScala
    println("Available Task Names: ")
    for (i <- 0 until taskNames.length) {
      println(i + ":\t" + taskNames(i))
    }
    println("")

    val taskIdx:Int = 0
    val varIdx:Int = 0

    val startTime = System.currentTimeMillis()

    val goldActions = getGoldPath(taskIdx, varIdx)

    println("Gold actions: " + goldActions.mkString(", "))

    // Find the set of actions that were used in the gold data
    val allowableActionStarts = mutable.Set[String]()
    for (goldAction <- goldActions) {
      val actionName = goldAction.split(" ")(0)
      allowableActionStarts.add(actionName)
    }

    this.crawlToDepth(taskIdx, varIdx, actionsSoFar = Array.empty[String], maxDepth = 3, allowableActionStarts.toSet, simplificationStr = "easy")

    val deltaTime = System.currentTimeMillis() - startTime
    println("Total execution time: " + deltaTime + " msec")


  }


}