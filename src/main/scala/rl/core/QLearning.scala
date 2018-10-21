package rl.core

import scala.util.Random

case class QLearning[State, Action](
    α: Double, // step size, 0.0 ≦ α ≦ 1.0, controls how much the agent updates its action-value function Q(s, a)
    γ: Double, // discount rate, 0.0 ≦ γ ≦ 1.0, controls how much the one-step backup affects Q(s, a)
    ε: Double, // 0.0 ≦ ε ≦ 1.0, probability of choosing a random action
    Q: Map[State, Map[Action, Double]] // the estimated action-value function Q(s, a)
)

object QLearning {

  implicit def agentBehaviour[State, Action]
    : AgentBehaviour[QLearning[State, Action], State, Action] =
    new AgentBehaviour[QLearning[State, Action], State, Action] {

      def chooseAction(
          agent: QLearning[State, Action],
          state: State,
          validActions: List[Action]): (Action, ActionResult[State] => QLearning[State, Action]) = {
        // Get Q(s, {a}), or initialise it arbitrarily to 0 for all actions if not initialised yet
        val actionValues = agent.Q.getOrElse(state, validActions.map(_ -> 0.0).toMap)

        // choose the next action
        val (chosenAction, currentActionValue) = epsilonGreedy(actionValues, agent.ε)

        // learn!
        val updateStateActionValue: ActionResult[State] => QLearning[State, Action] = {
          actionResult =>
            val nextStateActionValues =
              agent.Q.getOrElse(actionResult.nextState, validActions.map(_ -> 0.0).toMap)
            val maxNextStateActionValue =
              nextStateActionValues.values.fold(Double.MinValue)(_ max _)

            // Q(s_t, a_t) <- Q(s_t, a_t) + α (r_t+1 + γ max_a Q(s_t+1, a) - Q(s_t, a_t)
            val updatedActionValue =
              currentActionValue + agent.α * (actionResult.reward + agent.γ * maxNextStateActionValue - currentActionValue)

            val updatedActionValues = actionValues + (chosenAction -> updatedActionValue)
            val updatedQ            = agent.Q + (state             -> updatedActionValues)

            agent.copy(Q = updatedQ)
        }

        (chosenAction, updateStateActionValue)
      }

      /*
       ε-greedy: choose one of the actions with the highest value most of the time (i.e. exploit)
       but choose an action randomly some of the time (i.e. explore)
       */
      private def epsilonGreedy(actionValues: Map[Action, Double], ε: Double): (Action, Double) = {
        if (Random.nextDouble() < ε) {
          Random.shuffle(actionValues.toList).head
        } else {
          val sorted   = actionValues.toList.sortBy(_._2).reverse
          val maxValue = sorted.head._2
          Random.shuffle(sorted.takeWhile(_._2 == maxValue)).head
        }
      }

    }

}
