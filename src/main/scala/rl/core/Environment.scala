package rl.core

trait Environment[State, Action] {

  type Reward = Double

  def validActions(currentState: State): Set[Action]

  // TODO implicit or something to provide evidence that it's a valid action for the current state.
  // Idea: State has set of valid actions as a type member, and we require implicit evidence that actionTaken
  // is a member of that set?
  def step(currentState: State, actionTaken: Action): (State, Reward)

  def isTerminal(state: State): Boolean

}
