package rl.pacman.ui

import org.scalajs.dom
import org.scalajs.dom.html
import rl.core._
import rl.pacman.core.PacmanProblem
import rl.pacman.core.PacmanProblem._

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("PacmanUI")
object PacmanUI {

  private val initialAgentData: QLearning[GameState, Move] =
    QLearning(α = 0.1, γ = 1.0, ε = 0.1, Q = Map.empty)

  private val env: Environment[GameState, Move]                      = implicitly
  private val stateConversion: StateConversion[GameState, GameState] = implicitly
  private val agentBehaviour: AgentBehaviour[QLearning[GameState, Move], GameState, Move] =
    implicitly

  @JSExport
  def main(document: dom.Document, canvas: html.Canvas): Unit = {
    var agentData            = initialAgentData
    var gameState: GameState = initialState
    var episode              = 1

    def step(): Unit = {
      val currentState = stateConversion.convertState(gameState)
      val (nextAction, updateAgent) =
        agentBehaviour.chooseAction(agentData, currentState, validActions)
      val (nextState, reward) = env.step(gameState, nextAction)

      agentData = updateAgent(ActionResult(reward, stateConversion.convertState(nextState)))
      gameState = nextState

      gameState match {
        case normal: GameState.Normal =>
          drawGame(canvas, normal, nextAction)
        case GameState.Won | GameState.Lost =>
          episode += 1
          gameState = initialState
          drawGame(canvas, initialState, nextAction)
      }
    }

    dom.window.setInterval(() => step(), 500)
  }

  private def drawGame(canvas: html.Canvas,
                       state: PacmanProblem.GameState.Normal,
                       actionTaken: Move): Unit = {
    val ctx         = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val pixelWidth  = 50
    val pixelHeight = 50

    def drawGhost(ghost: Location, colour: String): Unit = {
      ctx.beginPath()
      ctx.fillStyle = colour
      ctx.arc(ghost.x * pixelWidth + 25, ghost.y * pixelHeight + 25, 20, 0.0, Math.PI * 2.0)
      ctx.fill()
      ctx.closePath()
    }

    ctx.fillStyle = "black"
    ctx.fillRect(0, 0, 1000, 350)

    // draw walls
    for (wall <- walls) {
      ctx.fillStyle = "blue"
      ctx.fillRect(wall.x * pixelWidth + 10,
                   wall.y * pixelHeight + 10,
                   pixelWidth - 20,
                   pixelHeight - 20)
    }

    // draw food
    for (food <- state.food) {
      ctx.beginPath()
      ctx.fillStyle = "yellow"
      ctx.arc(food.x * pixelWidth + 25, food.y * pixelHeight + 25, 5, 0.0, Math.PI * 2.0)
      ctx.fill()
      ctx.closePath()
    }

    // draw pills
    for (pill <- state.pills) {
      ctx.beginPath()
      ctx.fillStyle = "yellow"
      ctx.arc(pill.x * pixelWidth + 25, pill.y * pixelHeight + 25, 15, 0.0, Math.PI * 2.0)
      ctx.fill()
      ctx.closePath()
    }

    // draw ghosts
    state.mode match {
      case Mode.Normal =>
        drawGhost(state.ghost1, "green")
        drawGhost(state.ghost2, "red")
      case Mode.ChaseGhosts(_) =>
        drawGhost(state.ghost1, "cyan")
        drawGhost(state.ghost2, "cyan")
    }

    // draw pacman
    val (startAngle, endAngle) = actionTaken match {
      case Move.Left  => (Math.PI * -0.8, Math.PI * 0.8)
      case Move.Up    => (Math.PI * -0.3, Math.PI * 1.3)
      case Move.Right => (Math.PI * 0.2, Math.PI * 1.8)
      case Move.Down  => (Math.PI * 0.7, Math.PI * 2.3)
    }
    ctx.beginPath()
    ctx.fillStyle = "yellow"
    ctx.arc(state.pacman.x * pixelWidth + 25,
            state.pacman.y * pixelHeight + 25,
            20.0,
            startAngle,
            endAngle)
    ctx.lineTo(state.pacman.x * pixelWidth + 25, state.pacman.y * pixelHeight + 25)
    ctx.closePath()
    ctx.fill()
  }

}
