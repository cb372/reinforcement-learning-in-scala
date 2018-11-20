package rl.core

trait Environment[State, Action] {

  /**
    * Given the current state, what are the legal actions the agent can take?
    */
  def possibleActions(currentState: State): List[Action]

  /**
    * Given the current state and the action chosen by the agent,
    * what state does the agent move into and what reward does it get?
    *
    * Things to note:
    * - The reward might be positive, negative or zero.
    * - The next state might be the same as the current state.
    * - Both the state transition function and the reward function may be stochastic,
    *   meaning they follow some probability distribution and do not always
    *   give the same output for a given input.
    */
  def step(currentState: State, actionTaken: Action): (State, Reward)

  /**
    * Is the given state terminal or not?
    * For continuous (non-episodic) problems, this will always be false.
    */
  def isTerminal(state: State): Boolean

}
