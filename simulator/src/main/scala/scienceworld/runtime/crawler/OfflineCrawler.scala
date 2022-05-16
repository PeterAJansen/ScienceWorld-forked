package scienceworld.runtime.crawler

import main.scala.scienceworld.runtime.SimplifierProcessor
import scienceworld.runtime.crawler.OfflineCrawler.{interface, runActionSequence}
import scienceworld.runtime.pythonapi.PythonInterface

import scala.util.control.Breaks.{break, breakable}
import collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class ThreadedCrawler(val id:Int, val _taskIdx:Int, val _varIdx:Int, val _actionsSoFar:Array[String], val _maxDistance:Int, val _simplificationStr:String, val _goldPath:Array[String], val _goldPathSet:Set[String]) extends Thread {
  private var isRunning:Boolean = false
  private var isWinning:Boolean = false
  private var isCompleted:Boolean = false

  val verboseDebugOutput:Boolean = true

  // Set thread ID
  this.setName(id.toString)

  def isThreadRunning():Boolean = return this.isRunning

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
  def runActionSequence(taskIdx:Int, varIdx:Int, actionsToRun:Array[String], simplificationStr:String):(String, Double, Boolean, Array[String]) = {
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


  def crawlAroundGoldPath(taskIdx:Int, varIdx:Int, actionsSoFar:Array[String], maxDistance:Int, simplificationStr:String, goldPath:Array[String], goldPathSet:Set[String], spawnThreads:Boolean = false): Unit = {

    // Stop condition
    if (actionsSoFar.length > goldPath.size + maxDistance) return

    println("Path: " + actionsSoFar.mkString(", ") )

    // Get info for this node
    val (description, score, isCompleted, validActions) = runActionSequence(taskIdx, varIdx, actionsSoFar, simplificationStr)

    println ("Valid actions: " + validActions.mkString(", "))
    println ("Score: " + score)
    println ("")

    if (isCompleted) return

    for (actionStr <- validActions) {
      val newActionSeq = actionsSoFar ++ Array(actionStr)
      val newActionSeqSet = newActionSeq.toSet

      val differences = newActionSeqSet.diff(goldPathSet)
      if (differences.size <= maxDistance) {
        println("** Thread " + Thread.currentThread().getName() + " Calling: " + newActionSeq.mkString(", ") + " (differences: " + differences.mkString(", ") + ")")
        //crawlAroundGoldPath(taskIdx, varIdx, newActionSeq, maxDistance, simplificationStr, goldPath, goldPathSet)
        if (spawnThreads) {
          // This will probably explode, should likely never be used
          val th = new ThreadedCrawler(id = Random.nextInt(100000), taskIdx, varIdx, newActionSeq, maxDistance, simplificationStr, goldPath, goldPathSet)
          th.run()
          // TODO: Wait?

        } else {
          // Do not spawn a new thread, but use the current thread
          this.crawlAroundGoldPath(taskIdx, varIdx, newActionSeq, maxDistance, simplificationStr, goldPath, goldPathSet, spawnThreads = false)
        }

      } else {
        //## println("Gold Path: " + goldPath.mkString(", "))
        //## println("Differences exceed max length: " + differences.mkString(", "))
        //## println("")
      }


    }

  }

  override def run(): Unit = {
    this.isRunning = true
    if (verboseDebugOutput) println("Thread " + Thread.currentThread().getName() + " is running.")

    this.crawlAroundGoldPath(_taskIdx, _varIdx, actionsSoFar = Array.empty[String], _maxDistance, _simplificationStr, _goldPath, _goldPathSet)

    this.isRunning = false
    this.isCompleted = true
  }


}




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

    // Get info for this node
    val (description, score, isCompleted, validActions) = runActionSequence(taskIdx, varIdx, actionsSoFar, simplificationStr)

    println ("Valid actions: " + validActions.mkString(", "))

    // Stop condition
    if (actionsSoFar.length > maxDepth) return

    if (isCompleted) return

    for (actionStr <- validActions) {
      val actionStart = actionStr.split(" ")(0)   // Only continue with this action if it's on the list of allowed actions
      if (allowableActionStarts.contains(actionStart)) {
        crawlToDepth(taskIdx, varIdx, actionsSoFar ++ Array(actionStr), maxDepth, allowableActionStarts, simplificationStr)
      }

    }

  }


  def crawlAroundGoldPath(taskIdx:Int, varIdx:Int, actionsSoFar:Array[String], maxDistance:Int, simplificationStr:String, goldPath:Array[String], goldPathSet:Set[String]): Unit = {

    // Stop condition
    if (actionsSoFar.length > goldPath.size + maxDistance) return

    println("Path: " + actionsSoFar.mkString(", ") )

    // Get info for this node
    val (description, score, isCompleted, validActions) = runActionSequence(taskIdx, varIdx, actionsSoFar, simplificationStr)

    println ("Valid actions: " + validActions.mkString(", "))
    println ("Score: " + score)
    println ("")

    if (isCompleted) return

    for (actionStr <- validActions) {
      val newActionSeq = actionsSoFar ++ Array(actionStr)
      val newActionSeqSet = newActionSeq.toSet

      val differences = newActionSeqSet.diff(goldPathSet)
      if (differences.size <= maxDistance) {
        println("** Calling: " + newActionSeq.mkString(", ") + " (differences: " + differences.mkString(", ") + ")")

        // Normal, non-threaded
        crawlAroundGoldPath(taskIdx, varIdx, newActionSeq, maxDistance, simplificationStr, goldPath, goldPathSet)

      } else {
        println("Gold Path: " + goldPath.mkString(", "))
        println("Differences exceed max length: " + differences.mkString(", "))
        println("")
      }

    }

  }


  def crawlAroundGoldPathThreaded(taskIdx:Int, varIdx:Int, actionsSoFar:Array[String], maxDistance:Int, simplificationStr:String, goldPath:Array[String], goldPathSet:Set[String]): Unit = {

    // Stop condition
    if (actionsSoFar.length > goldPath.size + maxDistance) return

    println("Path: " + actionsSoFar.mkString(", ") )

    // Get info for this node
    val (description, score, isCompleted, validActions) = runActionSequence(taskIdx, varIdx, actionsSoFar, simplificationStr)

    println ("Valid actions: " + validActions.mkString(", "))
    println ("Score: " + score)
    println ("")

    if (isCompleted) return

    val threadsOut = new ArrayBuffer[(ThreadedCrawler, String)]()

    // Spool up threads for valid actions
    for (actionStr <- validActions) {
      val newActionSeq = actionsSoFar ++ Array(actionStr)
      val newActionSeqSet = newActionSeq.toSet

      val differences = newActionSeqSet.diff(goldPathSet)
      if (differences.size <= maxDistance) {
        println("** Calling: " + newActionSeq.mkString(", ") + " (differences: " + differences.mkString(", ") + ")")

        // Normal, non-threaded
        // crawlAroundGoldPath(taskIdx, varIdx, newActionSeq, maxDistance, simplificationStr, goldPath, goldPathSet)

        // Threaded
        val thread = new ThreadedCrawler(id = threadsOut.length, taskIdx, varIdx, newActionSeq, maxDistance, simplificationStr, goldPath, goldPathSet)
        threadsOut.append( (thread, actionStr) )
        thread.start()

      } else {
        //## println("Gold Path: " + goldPath.mkString(", "))
        //## println("Differences exceed max length: " + differences.mkString(", "))
        //## println("")
      }

    }


    // Wait for threads to finish
    var done:Boolean = false
    while (done == false) {

      done = true

      breakable {
        for (i <- 0 until threadsOut.length) {
          if (threadsOut(i)._1.isThreadRunning() == true) {
            println ("Thread " + i + " still running... ")
            done = false
            break()
          }
        }
      }

      Thread.sleep(500)
    }

    // If we reach here, the threads should all have completed

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

    println("")
    println("--------------------------------------------------------")
    println("--------------------------------------------------------")
    println("--------------------------------------------------------")
    println("")
    val taskDesc = interface.getTaskDescription()
    println ("Task Description: " + taskDesc)
    println("")
    println("--------------------------------------------------------")

    val simplificationStr = "easy"
    //val simplificationStr = "navigationOnly"    //## ~63 seconds
    //this.crawlToDepth(taskIdx, varIdx, actionsSoFar = Array.empty[String], maxDepth = 5, allowableActionStarts.toSet, simplificationStr)

    //def crawlAroundGoldPath(taskIdx:Int, varIdx:Int, actionsSoFar:Array[String], maxDistance:Int, simplificationStr:String, goldPath:Set[String]): Unit = {

    //this.crawlAroundGoldPath(taskIdx, varIdx, actionsSoFar = Array.empty[String], maxDistance = 1, simplificationStr, goldPath = goldActions, goldPathSet = goldActions.toSet)

    // Non-threaded
    //this.crawlAroundGoldPath(taskIdx, varIdx, actionsSoFar = Array.empty[String], maxDistance = 0, simplificationStr, goldPath = goldActions, goldPathSet = goldActions.toSet)

    // Threaded
    this.crawlAroundGoldPathThreaded(taskIdx, varIdx, actionsSoFar = Array.empty[String], maxDistance = 1, simplificationStr, goldPath = goldActions, goldPathSet = goldActions.toSet)

    val deltaTime = System.currentTimeMillis() - startTime
    println("Total execution time: " + deltaTime + " msec")


  }


}