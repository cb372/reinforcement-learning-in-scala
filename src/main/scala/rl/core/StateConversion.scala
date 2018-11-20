package rl.core

trait StateConversion[EnvState, AgentState] {

  /**
    * Convert from the "true", complete state as known by the environment,
    * into a simplified state that we give to the agent.
    *
    * This is a chance to do things:
    *
    * 1. If the problem includes any constraints that say the agent should have incomplete
    *    knowledge of the environment, we can encode that here.
    *
    * 2. We can discard some information in order to reduce the agent's state space,
    *    e.g. by bucketing a large number of environment states into a single agent state.
    */
  def convertState(envState: EnvState): AgentState

}
