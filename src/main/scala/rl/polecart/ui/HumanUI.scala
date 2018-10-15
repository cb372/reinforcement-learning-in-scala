package rl.polecart.ui

import org.scalajs.dom

import scala.util.Random
import org.scalajs.dom.html.Canvas
import rl.polecart.core.PoleBalancingProblem

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("PolecartHumanUI")
object HumanUI {

  sealed trait UIState
  case object Idle extends UIState
  case object Running extends UIState

  private val initialPoleCartState = PoleBalancingProblem.PoleCartState(0.0, 0.0, 0.0, 0.0)

  @JSExport
  def main(canvas: Canvas, infoLabel: dom.Element, timeLabel: dom.Element, statusLabel: dom.Element): Unit = {
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    def clear() = {
      ctx.fillStyle = "white"
      ctx.fillRect(0, 0, 600, 300)

      ctx.fillStyle = "black"
      ctx.strokeRect(0, 0, 600, 300)
      ctx.fillRect(0, 250, 50, 50)
      ctx.fillRect(550, 250, 50, 50)
    }

    def drawCart(state: PoleBalancingProblem.PoleCartState) = {
      val middleX = 300 + (state.cartPosition * 100)
      val leftX = middleX - 10
      val topY = 250
      ctx.fillStyle = "blue"
      ctx.fillRect(leftX, topY, 20, 40)
      // TODO wheels
      // TODO pole
    }

    var uiState: UIState = Idle

    var poleCartState = initialPoleCartState
    var currentAction = PoleBalancingProblem.PushCart.Left

    def tick = {
      clear()

      uiState match {
        case Idle =>
          infoLabel.textContent = "Press ← or → to start"
          drawCart(poleCartState)
        case Running =>
          // TODO keyboard events
          drawCart(poleCartState)
      }
//      for (i <- 0 until 10){
//        if (count % 3000 == 0) clear()
//        count += 1
//        p = (p + corners(Random.nextInt(3))) / 2
//
//        val height = 512.0 / (255 + p.y)
//        val r = (p.x * height).toInt
//        val g = ((255-p.x) * height).toInt
//        val b = p.y
//        ctx.fillStyle = s"rgb($g, $r, $b)"
//
//        ctx.fillRect(p.x, p.y, 1, 1)
//      }
    }

    dom.window.setInterval(() => tick, 20)
  }

}
