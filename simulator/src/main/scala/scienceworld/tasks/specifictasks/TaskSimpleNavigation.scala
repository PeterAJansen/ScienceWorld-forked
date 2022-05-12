package scienceworld.tasks.specifictasks

import scienceworld.goldagent.PathFinder
import scienceworld.objects.agent.Agent
import scienceworld.runtime.pythonapi.PythonInterface
import scienceworld.struct.EnvObject
import scienceworld.tasks.{Task, TaskDisable, TaskMaker1, TaskModifier, TaskObject, TaskValueStr}
import scienceworld.tasks.goals.{Goal, GoalSequence}
import scienceworld.tasks.goals.specificgoals.{GoalChangeStateOfMatter, GoalFocusOnAnimal, GoalFocusOnLivingThing, GoalFocusOnNonlivingThing, GoalFocusOnPlant, GoalInRoomWithOpenDoor, GoalIsDifferentStateOfMatter, GoalIsNotStateOfMatter, GoalIsStateOfMatter, GoalMoveToLocation, GoalMoveToNewLocation, GoalObjectInContainer}
import scienceworld.tasks.specifictasks.TaskSimpleNavigation.MODE_NAVIGATE

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}


class TaskSimpleNavigation(val mode:String = MODE_NAVIGATE) extends TaskParametric {
  val taskName = "task-11-" + mode.replaceAll(" ", "-")

  // Variation 1: Location to move to
  val locationPossibilities = new ArrayBuffer[ Array[TaskModifier] ]()
  val locations = Array("kitchen", "bathroom", "living room", "bedroom", "workshop", "outside", "green house", "foundry", "hallway", "art studio")
  for (location <- locations) {
    locationPossibilities.append(Array( new TaskValueStr("location", location) ) )
  }

  // Combinations
  val combinations = for {
    i <- locationPossibilities
  } yield List(i)

  println("Number of combinations: " + combinations.length)

  def numCombinations():Int = this.combinations.size

  def getCombination(idx:Int):Array[TaskModifier] = {
    val out = new ArrayBuffer[TaskModifier]
    for (elem <- combinations(idx)) {
      out.insertAll(out.length, elem)
    }
    // Return
    out.toArray
  }

  // Setup a particular modifier combination on the universe
  private def setupCombination(modifiers:Array[TaskModifier], universe:EnvObject, agent:Agent):(Boolean, String) = {
    // Run each modifier's change on the universe
    for (mod <- modifiers) {
      println("Running modifier: " + mod.toString)
      val success = mod.runModifier(universe, agent)
      if (!success) {
        return (false, "ERROR: Error running one or more modifiers while setting up task environment.")
      }
    }
    // If we reach here, success
    return (true, "")
  }

  def setupCombination(combinationNum:Int, universe:EnvObject, agent:Agent): (Boolean, String) = {
    if (combinationNum >= this.numCombinations()) {
      return (false, "ERROR: The requested variation (" + combinationNum + ") exceeds the total number of variations (" + this.numCombinations() + ").")
    }
    return this.setupCombination( this.getCombination(combinationNum), universe, agent )
  }


  // Setup a set of subgoals for this task modifier combination.
  private def setupGoals(modifiers:Array[TaskModifier], combinationNum:Int): Task = {
    // Step 1: Find substance name
    val location = this.getTaskValueStr(modifiers, "location")

    var subTask = ""
    val gSequence = new ArrayBuffer[Goal]
    val gSequenceUnordered = new ArrayBuffer[Goal]
    if (mode == MODE_NAVIGATE) {
      subTask = "living thing"
      gSequence.append( new GoalMoveToLocation(location.get, _isOptional = false, description = "Move to the task location") )

      gSequenceUnordered.append( new GoalMoveToNewLocation(_isOptional = true, description = "Move to a new location") )            // Move to any new location
    }
    val taskLabel = taskName + "-variation" + combinationNum
    val description = "Your task is to move to the " + location.get + "."
    val goalSequence = new GoalSequence(gSequence.toArray, gSequenceUnordered.toArray)

    val task = new Task(taskName, description, goalSequence, taskModifiers = modifiers)

    // Return
    return task
  }

  def setupGoals(combinationNum:Int): Task = {
    this.setupGoals( this.getCombination(combinationNum), combinationNum )
  }

  def mkGoldActionSequence(modifiers:Array[TaskModifier], runner:PythonInterface): (Boolean, Array[String]) = {
    if (mode == MODE_NAVIGATE) {
      return mkGoldActionSequenceSimpleNavigation(modifiers, runner)
    } else {
      throw new RuntimeException("ERROR: Unrecognized task mode: " + mode)
    }

  }

  /*
   * Gold action sequences
   */
  def mkGoldActionSequenceSimpleNavigation(modifiers:Array[TaskModifier], runner:PythonInterface): (Boolean, Array[String]) = {
    val location = this.getTaskValueStr(modifiers, "location").get

    val universe = runner.agentInterface.get.universe
    val agent = runner.agentInterface.get.agent


    // Step 1: Move from starting location to task location
    val startLocation = agent.getContainer().get.name
    val (actions, actionStrs) = PathFinder.createActionSequence(universe, agent, startLocation, endLocation = location)
    runActionSequence(actionStrs, runner)

    // Step 1A: Look around
    val (actionLook, actionLookStr) = PathFinder.actionLookAround(agent)
    runAction(actionLookStr, runner)

    // Return
    return (true, getActionHistory(runner))
  }

}


object TaskSimpleNavigation {
  val MODE_NAVIGATE       = "navigate to location"


  def registerTasks(taskMaker:TaskMaker1): Unit = {
    taskMaker.addTask( new TaskSimpleNavigation(mode = MODE_NAVIGATE) )
  }

}

