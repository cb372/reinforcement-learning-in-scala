package rl.polecart.ui

import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import rl.polecart.core.PoleBalancingProblem

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("PolecartHumanUI")
object HumanUI {

  sealed trait UIState
  case object Idle    extends UIState
  case object Running extends UIState

  private val initialPoleCartState: PoleBalancingProblem.PoleCartState =
    PoleBalancingProblem.PoleCartState(0.0, 0.0, 0.0, 0.0)

  @JSExport
  def main(window: dom.Window, canvas: Canvas, infoLabel: dom.Element): Unit = {
    var uiState: UIState = Idle

    var poleCartState: PoleBalancingProblem.PoleCartState = initialPoleCartState
    var currentAction: PoleBalancingProblem.PushCart =
      PoleBalancingProblem.PushCart.Left
    var timeElapsed    = 0.0
    var maxTimeElapsed = 0.0

    def tick(): Unit = {
      clear(canvas)

      uiState match {
        case Idle =>
          drawCart(canvas, poleCartState, timeElapsed)
        case Running =>
          timeElapsed += 0.02
          poleCartState = PoleBalancingProblem.environment
            .step(poleCartState, currentAction)
            ._1
          drawCart(canvas, poleCartState, timeElapsed)
          if (PoleBalancingProblem.environment.isTerminal(poleCartState)) {
            failed()
          }
      }
    }

    def running(): Unit = {
      infoLabel.textContent = ""
      poleCartState = initialPoleCartState
      uiState = Running
    }

    def failed(): Unit = {
      maxTimeElapsed = maxTimeElapsed max timeElapsed
      infoLabel.textContent =
        f"FAILED! You lasted $timeElapsed%.2f seconds. Your record is $maxTimeElapsed%.2f seconds. Press ← or → to try again"
      timeElapsed = 0.0
      uiState = Idle
    }

    window.onkeydown = { event =>
      event.key match {
        case "ArrowLeft" =>
          currentAction = PoleBalancingProblem.PushCart.Left
          running()
        case "ArrowRight" =>
          currentAction = PoleBalancingProblem.PushCart.Right
          running()
        case other =>
        // ignore
      }
    }

    infoLabel.textContent = "Press ← or → to start"

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

    // time counter in top-left corner
    ctx.beginPath()
    ctx.strokeStyle = "black"
    ctx.lineWidth = 1
    ctx.strokeText(f"t = $timeElapsed%.2f", 10, 20)
    ctx.closePath()
  }

}
