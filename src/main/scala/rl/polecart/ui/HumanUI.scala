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
      // clear the canvas
      ctx.fillStyle = "white"
      ctx.clearRect(0, 0, canvas.width, canvas.height)

      // border
      ctx.lineWidth = 3
      ctx.strokeStyle = "black"
      ctx.fillStyle = "black"
      ctx.strokeRect(0, 0, canvas.width, canvas.height)

      // walls
      val wallWidth = 50
      val wallHeight = 50
      val wallTop = canvas.height - wallHeight
      ctx.fillRect(0, wallTop, wallWidth, wallHeight)
      ctx.fillRect(canvas.width - wallWidth, wallTop, wallWidth, wallHeight)
    }

    def drawCart(state: PoleBalancingProblem.PoleCartState) = {
      val cartTopY = canvas.height - 50
      val cartWidth = 70
      val cartHeight = 30
      val cartMiddleX = 325 + (state.cartPosition * 100)
      val cartLeftX = cartMiddleX - cartWidth / 2

      val wheelY = canvas.height - 10
      val leftWheelX = cartMiddleX - 20
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
      val poleX = cartMiddleX
      val poleBottomY = cartTopY - 5
      val poleTopY = poleBottomY - 50

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
      ctx.setTransform(1,0,0,1,0,0)
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
