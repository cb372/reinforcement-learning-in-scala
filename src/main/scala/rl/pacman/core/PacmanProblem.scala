package rl.pacman.core

import rl.core._

import scala.util.Random

object PacmanProblem {

  // Note: x ranges from 0 to 19, y ranges from 0 to 6
  case class Location(x: Int, y: Int) {

    def move(move: Move): Location = move match {
      case Move.Left  => Location(x - 1, y)
      case Move.Right => Location(x + 1, y)
      case Move.Up    => Location(x, y - 1)
      case Move.Down  => Location(x, y + 1)
    }

  }

  // convenience method for constructing a Location
  private def xy(x: Int, y: Int) = Location(x, y)

  // the current game mode: are the ghosts chasing Pacman or vice versa?
  sealed trait Mode { def chasingGhosts: Boolean }
  object Mode {
    case object Normal                         extends Mode { val chasingGhosts = false }
    case class ChaseGhosts(timeRemaining: Int) extends Mode { val chasingGhosts = true  }
  }

  /*
  The complete state of the game:
  - the location of each ghost
  - Pacman's location
  - the locations of all remaining food
  - the locations of all remaining pills
  - the current game mode
   */
  case class GameState(
      ghost1: Location,
      ghost2: Location,
      pacman: Location,
      food: Set[Location],
      pills: Set[Location],
      mode: Mode
  )

  // the actions that the agent can take to move Pacman
  sealed trait Move
  object Move {
    case object Up    extends Move
    case object Down  extends Move
    case object Left  extends Move
    case object Right extends Move
  }

  val allActions: List[Move] = List(Move.Up, Move.Down, Move.Left, Move.Right)

  /*
  We use the following "smallClassic" grid:

   0123456789
  0%%%%%%%%%%%%%%%%%%%%
  1%......%G  G%......%
  2%.%%...%%  %%...%%.%
  3%.%o.%........%.o%.%
  4%.%%.%.%%%%%%.%.%%.%
  5%........P.........%
  6%%%%%%%%%%%%%%%%%%%%

  % = wall
  . = food
  o = pill
  G = ghost start location
  P = Pacman start location
   */

  val walls: Set[Location] =
    // format: off
    List.tabulate(20)(xy(_, 0)).toSet ++ // top wall
      Set(xy(0, 1), xy(7, 1), xy(12, 1), xy(19, 1)) ++
      Set(xy(0, 2), xy(2, 2), xy(3, 2), xy(7, 2), xy(8, 2), xy(11, 2), xy(12, 2), xy(16, 2), xy(17, 2), xy(19, 2)) ++
      Set(xy(0, 3), xy(2, 3), xy(5, 3), xy(14, 3), xy(17, 3), xy(19, 3)) ++
      Set(xy(0, 4), xy(2, 4), xy(3, 4), xy(5, 4), xy(7, 4), xy(8, 4), xy(9, 4), xy(10, 4), xy(11, 4), xy(12, 4), xy(14, 4), xy(16, 4), xy(17, 4), xy(19, 4)) ++
      Set(xy(0, 5), xy(19, 5)) ++
      List.tabulate(20)(xy(_, 6)).toSet // bottom wall
  // format: on

  private val initialGhost1 = xy(8, 1)
  private val initialGhost2 = xy(11, 1)
  private val initialPacman = xy(9, 5)
  private val initialPills  = Set(xy(3, 3), xy(16, 3))

  private val initialFood: Set[Location] =
    // format: off
      Set(xy(1, 1), xy(2, 1), xy(3, 1), xy(4, 1), xy(5, 1), xy(6, 1), xy(13, 1), xy(14, 1), xy(15, 1), xy(16, 1), xy(17, 1), xy(18, 1)) ++
      Set(xy(1, 2), xy(4, 2), xy(5, 2), xy(6, 2), xy(13, 2), xy(14, 2), xy(15, 2), xy(18, 2)) ++
      Set(xy(1, 3), xy(4, 3), xy(6, 3), xy(7, 3), xy(8, 3), xy(9, 3), xy(10, 3), xy(11, 3), xy(12, 3), xy(13, 3), xy(15, 3), xy(18, 3)) ++
      Set(xy(1, 4), xy(4, 4), xy(6, 4), xy(13, 4), xy(15, 4), xy(18, 4)) ++
      Set(xy(1, 5), xy(2, 5), xy(3, 5), xy(4, 5), xy(5, 5), xy(6, 5), xy(7, 5), xy(8, 5), xy(10, 5), xy(11, 5), xy(12, 5), xy(13, 5), xy(14, 5), xy(15, 5), xy(16, 5), xy(17, 5), xy(18, 5))
  // format: on

  val initialState: GameState = GameState(
    ghost1 = initialGhost1,
    ghost2 = initialGhost2,
    pacman = initialPacman,
    food = initialFood,
    pills = initialPills,
    mode = Mode.Normal
  )

  implicit val environment: Environment[GameState, Move] =
    new Environment[GameState, Move] {

      override def possibleActions(currentState: GameState): List[Move] =
        allActions.filterNot(move => walls.contains(currentState.pacman.move(move)))

      override def step(currentState: GameState, actionTaken: Move): (GameState, Reward) = {
        // Calculate Pacman's new location, based on actionTaken and adjacent walls
        val nextPacmanLocation = updatePacmanLocation(currentState.pacman, actionTaken)

        // Calculate ghosts' new locations, based on their current locations and directions
        val nextGhost1 = updateGhost(currentState.ghost1, nextPacmanLocation, currentState.mode)
        val nextGhost2 = updateGhost(currentState.ghost2, nextPacmanLocation, currentState.mode)

        // Check if Pacman ate some food by moving to his new location
        val (ateFood, updatedFoodLocations) = {
          if (currentState.food.contains(nextPacmanLocation))
            (true, currentState.food - nextPacmanLocation)
          else
            (false, currentState.food)
        }

        val (atePill, updatedPillLocations) = {
          if (currentState.pills.contains(nextPacmanLocation))
            (true, currentState.pills - nextPacmanLocation)
          else
            (false, currentState.pills)
        }

        // If current mode is ChaseGhosts, decrement its timer. If it reaches zero, switch back to Normal.
        val updatedMode = {
          if (atePill)
            Mode.ChaseGhosts(timeRemaining = 40)
          else
            currentState.mode match {
              case Mode.Normal         => Mode.Normal
              case Mode.ChaseGhosts(0) => Mode.Normal
              case Mode.ChaseGhosts(t) => Mode.ChaseGhosts(t - 1)
            }
        }

        // Check if Pacman caught any ghosts
        val pacmanTouchingGhost1 = nextPacmanLocation == nextGhost1
        val updatedGhost1 =
          if (pacmanTouchingGhost1 && updatedMode.chasingGhosts)
            initialGhost1
          else
            nextGhost1

        val pacmanTouchingGhost2 = nextPacmanLocation == nextGhost2
        val updatedGhost2 =
          if (pacmanTouchingGhost2 && updatedMode.chasingGhosts)
            initialGhost2
          else
            nextGhost2

        val pacmanTouchingAGhost = pacmanTouchingGhost1 || pacmanTouchingGhost2
        val pacmanCaughtByGhost  = pacmanTouchingAGhost && !updatedMode.chasingGhosts
        val pacmanCaughtAGhost   = pacmanTouchingAGhost && updatedMode.chasingGhosts

        val nextState = GameState(
          ghost1 = updatedGhost1,
          ghost2 = updatedGhost2,
          pacman = nextPacmanLocation,
          food = updatedFoodLocations,
          pills = updatedPillLocations,
          mode = updatedMode
        )

        val reward = {
          if (pacmanCaughtByGhost)
            -100.0
          else if (ateFood)
            1.0
          else if (atePill)
            10.0
          else if (pacmanCaughtAGhost)
            50.0
          else
            0.0
        }

        (nextState, reward)
      }

      override def isTerminal(state: GameState): Boolean =
        state.food.isEmpty || isGameOver(state)

      private def isGameOver(state: GameState): Boolean = {
        val pacmanTouchingGhost = state.pacman == state.ghost1 || state.pacman == state.ghost2
        pacmanTouchingGhost && !state.mode.chasingGhosts
      }

      private def updatePacmanLocation(pacman: Location, move: Move): Location = {
        val next = pacman.move(move)
        if (walls.contains(next))
          // can't move into a wall, so stay where you are
          pacman
        else
          next
      }

      private def updateGhost(ghost: Location, pacman: Location, mode: Mode): Location = {
        if (ghost == pacman && !mode.chasingGhosts) {
          // if you've caught Pacman, stay where you are!
          ghost
        } else {
          val smartMoveProb = 0.8

          val validPositions = allActions.map(ghost.move).filterNot(walls.contains)

          if (Random.nextDouble() < smartMoveProb) {
            // make a "smart" move, i.e. either chase Pacman or run away from him depending on the game mode
            val sortedByDistance = validPositions
              .map(location => (location, manhattanDist(location, pacman)))
              .sortBy {
                case (_, distance) =>
                  if (mode.chasingGhosts)
                    distance * -1 // the further from Pacman the better
                  else
                    distance // the closer the better
              }
            val bestDistance  = sortedByDistance.head._2
            val bestPositions = sortedByDistance.takeWhile(_._2 == bestDistance)
            Random.shuffle(bestPositions).head._1
          } else {
            Random.shuffle(validPositions).head
          }
        }
      }

    }

  /*
  The ghosts use Manhattan distance when chasing Pacman.
  You might find it handy for your Pacman agent as well.
   */
  private def manhattanDist(from: Location, to: Location): Int =
    Math.abs(from.x - to.x) + Math.abs(from.y - to.y)

  /*
  TODO: Define a suitable agent state, and the conversion from `GameState` to `AgentState`.

  The trick is to find a way of encoding enough information about the game state
  without the number of states exploding.
  e.g. if you were to track the exact locations of Pacman and both ghosts,
  you already have 65 x 65 x 65 = 274,675 states to deal with.

  Your state encoding should also make sense when combined with the reward function.
  For example, the environment gives a reward when Pacman eats food, so intuitively
  the state should track food in some way.
   */
  //case class AgentState(...)
  type AgentState = GameState

  implicit val stateConversion: StateConversion[GameState, AgentState] = { gameState =>
    gameState
  }

}
