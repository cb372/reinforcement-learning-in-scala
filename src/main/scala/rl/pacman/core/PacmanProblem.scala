package rl.pacman.core

import rl.core._

object PacmanProblem {

  // Note: x ranges from 0 to 19, y ranges from 0 to 10
  case class Location(x: Int, y: Int)

  case class Ghost(location: Location, direction: Move)

  sealed trait Mode
  object Mode {
    case object Normal extends Mode
    case class ChaseGhosts(timeRemaining: Int) extends Mode
  }

  case class PacmanState(
                          ghost1: Ghost,
                          ghost2: Ghost,
                          pacman: Location,
                          food: Set[Location],
                          pills: Set[Location],
                          mode: Mode
                        )

  sealed trait Move
  object Move {
    case object Up    extends Move
    case object Down  extends Move
    case object Left  extends Move
    case object Right extends Move
  }

  val validActions: List[Move] = List(Move.Up, Move.Down, Move.Left, Move.Right)

  /*
  "smallClassic" grid:

  %%%%%%%%%%%%%%%%%%%%
  %......%G  G%......%
  %.%%...%%  %%...%%.%
  %.%o.%........%.o%.%
  %.%%.%.%%%%%%.%.%%.%
  %........P.........%
  %%%%%%%%%%%%%%%%%%%%

   */

  implicit val environment: Environment[PacmanState, Move] =
    new Environment[PacmanState, Move] {

      override def step(currentState: PacmanState, actionTaken: Move): (PacmanState, Reward) = {

        /*
        If current mode is ChaseGhosts, decrement its timer. If it reaches zero, switch back to Normal.

        Calculate Pacman's new location, based on actionTaken and adjacent walls.

        Then calculate ghosts' new locations, based on their current locations and directions.
        Change their direction if they hit a wall.
        Randomly change their direction occasionally. Or make them follow Pacman in some fashion?

        Check if there is food in Pacman's new location.
        If so, give positive reward and remove the food.

        Check if there is a pill in Pacman's new location.
        If so, give large positive reward, remove the pill and change the mode to ChaseGhosts(t = 100).

        Check if there is a ghost in Pacman's new location.
        If there is:
        - if new mode is Normal, give large negative reward. End of episode.
        - else (new mode is ChaseGhosts), give large positive reward and move ghost to its starting position.
         */
        val nextState = ???

        val reward = if (isTerminal(nextState)) -1 else 0

        (nextState, reward)
      }

      override def isTerminal(state: PacmanState): Boolean =
        isLevelComplete(state.food) || isCaught(state.ghost1, state.ghost2, state.pacman)

      private def isLevelComplete(food: Set[Location]) = food.isEmpty

      private def isCaught(ghost1: Ghost, ghost2: Ghost, pacman: Location) =
        ghost1.location == pacman || ghost2.location == pacman

    }
}
