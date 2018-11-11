package rl.gridworld.core

import rl.core.{Environment, Reward, StateConversion}

object GridworldProblem {

  // Note: x and y range from 0 to 4, not 1 to 5
  case class AgentLocation(x: Int, y: Int)

  sealed trait Move
  object Move {
    case object Up    extends Move
    case object Down  extends Move
    case object Left  extends Move
    case object Right extends Move
  }

  val allActions: List[Move] = List(Move.Up, Move.Down, Move.Left, Move.Right)

  implicit val environment: Environment[AgentLocation, Move] =
    new Environment[AgentLocation, Move] {

      override def possibleActions(currentState: AgentLocation): List[Move] =
        GridworldProblem.allActions

      override def step(currentLocation: AgentLocation,
                        actionTaken: Move): (AgentLocation, Reward) = currentLocation match {
        case AgentLocation(1, 0) =>
          // special cell A: regardless of action, jump to A' and receive 10 reward
          (AgentLocation(1, 4), 10.0)
        case AgentLocation(3, 0) =>
          // special cell B: regardless of action, jump to B' and receive 5 reward
          (AgentLocation(3, 2), 5.0)
        case AgentLocation(x, y) if wouldLeaveBoard(x, y, actionTaken) =>
          (currentLocation, -1.0)
        case AgentLocation(x, y) =>
          val newLocation = actionTaken match {
            case Move.Up    => AgentLocation(x, y - 1)
            case Move.Down  => AgentLocation(x, y + 1)
            case Move.Left  => AgentLocation(x - 1, y)
            case Move.Right => AgentLocation(x + 1, y)
          }
          (newLocation, 0.0)
      }

      private def wouldLeaveBoard(x: Int, y: Int, move: Move): Boolean =
        (x == 0 && move == Move.Left) ||
          (x == 4 && move == Move.Right) ||
          (y == 0 && move == Move.Up) ||
          (y == 4 && move == Move.Down)

      override def isTerminal(state: AgentLocation): Boolean =
        false // this is a continuous (non-episodic) problem

    }

}
