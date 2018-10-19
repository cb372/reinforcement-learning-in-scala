package rl.core

/**
  * The results of the agent taking an action:
  * it receives a reward and ends up in a new state.
  */
case class ActionResult[State](reward: Reward, nextState: State)
