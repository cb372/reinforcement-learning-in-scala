package rl.polecart.ui

import org.scalajs.dom
import org.scalajs.dom.html.{Canvas, Button}
import rl.core._
import rl.polecart.core.PoleBalancingProblem
import rl.polecart.core.PoleBalancingProblem._

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("PolecartQLearningUI")
object QLearningUI {

  sealed trait UIState
  case object Idle           extends UIState
  case object Stepping       extends UIState
  case object RunningEpisode extends UIState
  case object RunningForever extends UIState

  private val initialPoleCartState: PoleCartState =
    PoleBalancingProblem.PoleCartState(0.0, 0.0, 0.0, 0.0)

  private val initialAgentData: QLearning[RoughPoleCartState, PushCart] =
    QLearning(α = 0.1, γ = 1.0, ε = 0.1, Q = Map.empty)

  private val env: Environment[PoleCartState, PushCart]                           = implicitly
  private val stateConversion: StateConversion[PoleCartState, RoughPoleCartState] = implicitly
  private val agentBehaviour
    : AgentBehaviour[QLearning[RoughPoleCartState, PushCart], RoughPoleCartState, PushCart] =
    implicitly

  @JSExport
  def main(document: dom.Document,
           canvas: Canvas,
           infoLabel: dom.Element,
           stepButton: Button,
           runEpisodeButton: Button,
           runForeverButton: Button,
           pauseButton: Button): Unit = {
    var uiState: UIState = Idle

    var agentData      = initialAgentData
    var poleCartState  = initialPoleCartState
    var timeElapsed    = 0.0
    var maxTimeElapsed = 0.0
    var episodeCount   = 1

    def step(): Unit = {
      timeElapsed += 0.02

      val currentState = stateConversion.convertState(poleCartState)
      val (nextAction, updateAgent) =
        agentBehaviour.chooseAction(agentData, currentState, validActions)
      val (nextState, reward) = env.step(poleCartState, nextAction)

      agentData = updateAgent(ActionResult(reward, stateConversion.convertState(nextState)))
      poleCartState = nextState

      drawCart(canvas, poleCartState, episodeCount, timeElapsed)
      updateTable(document, agentData.Q)
    }

    def endOfEpisode(): Unit = {
      maxTimeElapsed = maxTimeElapsed max timeElapsed
      timeElapsed = 0.0
      episodeCount += 1
      poleCartState = initialPoleCartState
      infoLabel.textContent = f"Longest episode so far: $maxTimeElapsed%.2f seconds"
    }

    def tick(): Unit = {
      clear(canvas)

      uiState match {
        case Idle =>
          drawCart(canvas, poleCartState, episodeCount, timeElapsed)

        case Stepping =>
          step()
          if (env.isTerminal(poleCartState)) {
            endOfEpisode()
          }
          uiState = Idle

        case RunningEpisode =>
          step()
          if (env.isTerminal(poleCartState)) {
            endOfEpisode()
            uiState = Idle
          }

        case RunningForever =>
          step()
          if (env.isTerminal(poleCartState)) {
            endOfEpisode()
          }
      }
    }

    stepButton.onclick = _ => uiState = Stepping
    runEpisodeButton.onclick = _ => uiState = RunningEpisode
    runForeverButton.onclick = _ => uiState = RunningForever
    pauseButton.onclick = _ => uiState = Idle

    dom.window.setInterval(() => tick(), 20)
  }

  private def clear(canvas: Canvas): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    // clear the canvas
    ctx.fillStyle = "white"
    ctx.clearRect(0, 0, canvas.width, canvas.height)

    // border
    ctx.lineWidth = 3
    ctx.strokeStyle = "black"
    ctx.fillStyle = "black"
    ctx.strokeRect(0, 0, canvas.width, canvas.height)

    // walls
    val wallWidth  = 50
    val wallHeight = 50
    val wallTop    = canvas.height - wallHeight
    ctx.fillRect(0, wallTop, wallWidth, wallHeight)
    ctx.fillRect(canvas.width - wallWidth, wallTop, wallWidth, wallHeight)
  }

  private def drawCart(canvas: Canvas,
                       state: PoleBalancingProblem.PoleCartState,
                       episodeCount: Int,
                       timeElapsed: Double): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    val cartTopY    = canvas.height - 50
    val cartWidth   = 70
    val cartHeight  = 30
    val cartMiddleX = 325 + (state.cartPosition * 100)
    val cartLeftX   = cartMiddleX - cartWidth / 2

    val wheelY      = canvas.height - 10
    val leftWheelX  = cartMiddleX - 20
    val rightWheelX = cartMiddleX + 20

    // cart
    ctx.fillStyle = "blue"
    ctx.fillRect(cartLeftX, cartTopY, cartWidth, cartHeight)

    // left wheel
    ctx.beginPath()
    ctx.fillStyle = "blue"
    ctx.arc(leftWheelX, wheelY, 10.0, 0.0, 2 * Math.PI)
    ctx.fill()
    ctx.closePath()

    // right wheel
    ctx.beginPath()
    ctx.fillStyle = "blue"
    ctx.arc(rightWheelX, wheelY, 10.0, 0.0, 2 * Math.PI)
    ctx.fill()
    ctx.closePath()

    // pole
    val poleX       = cartMiddleX
    val poleBottomY = cartTopY - 5
    val poleTopY    = poleBottomY - 50

    ctx.beginPath()

    ctx.translate(poleX, poleBottomY)
    ctx.rotate(state.poleAngle)
    ctx.translate(-poleX, -poleBottomY)

    ctx.strokeStyle = "green"
    ctx.moveTo(poleX, poleBottomY)
    ctx.lineTo(poleX, poleTopY)
    ctx.lineWidth = 6
    ctx.stroke()

    ctx.closePath()

    // reset transform
    ctx.setTransform(1, 0, 0, 1, 0, 0)

    ctx.beginPath()
    ctx.strokeStyle = "black"
    ctx.lineWidth = 1
    ctx.strokeText(s"Episode $episodeCount", 10, 20)
    ctx.strokeText(f"t = $timeElapsed%.2f", 10, 40)
    ctx.closePath()
  }

  private def updateTable(
      document: dom.Document,
      Q: Map[PoleBalancingProblem.RoughPoleCartState, Map[PoleBalancingProblem.PushCart, Double]])
    : Unit = {
    def lower(x: Any): String = x.toString.toLowerCase.replaceAllLiterally(" ", "")

    for {
      cartPos <- List(RoughCartPosition.Left, RoughCartPosition.Middle, RoughCartPosition.Right)
      cartVel <- List(RoughCartVelocity.FastLeft,
                      RoughCartVelocity.Slow,
                      RoughCartVelocity.FastRight)
      poleVel <- List(RoughPoleVelocity.FastLeft,
                      RoughPoleVelocity.Slow,
                      RoughPoleVelocity.FastRight)
      poleAngle <- List(
        RoughPoleAngle.VeryLeft,
        RoughPoleAngle.QuiteLeft,
        RoughPoleAngle.SlightlyLeft,
        RoughPoleAngle.SlightlyRight,
        RoughPoleAngle.QuiteRight,
        RoughPoleAngle.VeryRight
      )
    } {
      val actionValues =
        Q.getOrElse(RoughPoleCartState(cartPos, cartVel, poleAngle, poleVel), Map.empty)
      val text = (actionValues.get(PushCart.Left), actionValues.get(PushCart.Right)) match {
        case (Some(l), Some(r)) => f"L: $l%.4f, R: $r%.4f"
        case _                  => ""
      }

      val id = s"${lower(cartPos)}_${lower(cartVel)}_${lower(poleVel)}_${lower(poleAngle)}"
      document.getElementById(id).innerHTML = text
    }
  }

}
