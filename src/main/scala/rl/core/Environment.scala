package rl.core

trait Environment[State, Action] {

  def possibleActions(currentState: State): List[Action]

  def step(currentState: State, actionTaken: Action): (State, Reward)

  def isTerminal(state: State): Boolean

}
