package rl.core

trait Environment[State, Action] {

  def step(currentState: State, actionTaken: Action): (State, Reward)

  def isTerminal(state: State): Boolean

}
