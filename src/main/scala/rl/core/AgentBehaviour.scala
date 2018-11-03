package rl.core

trait AgentBehaviour[AgentData, State, Action] {

  /**
    * Given an agent and the current state, asks the agent to choose the next action.
    *
    * Returns two things:
    *
    * 1. the action that the agent chose
    * 2. a function that, given the results of taking the action,
    *    uses it to improve the agent's policy and thus returns a new version of the agent
    */
  def chooseAction(agentData: AgentData,
                   state: State,
                   validActions: List[Action]): (Action, ActionResult[State] => AgentData)

}
