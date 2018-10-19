package rl.polecart.core

import rl.core.{Environment, QLearning, Reward, StateConversion}
import java.lang.Math._

object PoleBalancingProblem {

  case class PoleCartState(
      cartPosition: Double, // metres from start position (middle of cart, -ve is left, +ve is right)
      cartVelocity: Double, // m/s
      poleAngle: Double, // radians, angle from vertical
      poleVelocity: Double // radians/second, angular velocity
  ) {
    override def toString: String =
      s"""Pole-cart:
         |x  = $cartPosition
         |x' = $cartVelocity
         |θ  = ${toDegrees(poleAngle)}
         |θ' = ${toDegrees(poleVelocity)}
       """.stripMargin
  }

  sealed trait PushCart
  object PushCart {
    case object Left  extends PushCart
    case object Right extends PushCart
  }

  val validActions: List[PushCart] = List(PushCart.Left, PushCart.Right)

  implicit val environment: Environment[PoleCartState, PushCart] =
    new Environment[PoleCartState, PushCart] {

      override def step(currentState: PoleCartState,
                        actionTaken: PushCart): (PoleCartState, Reward) = {
        /*
      First we use non-linear differential equations to calculate the double derivatives
      x'' and θ'' of the cart position (x) and pole angle (θ) at time t,
      given x, θ, x' and θ' at time t.

      See the appendix of the paper "Neuronlike Adaptive Elements That Can Solve Difficult Learning Problems"
      (Barto, Sutton and Anderson, 1983) for the details of the differential equations.

      Once we have x'' and θ'' at time t, we use Euler's method (with a time step of 0.02 seconds)
      to estimate x' and θ' at time t+1:

      x'(t+1) = x'(t) + 0.02 * x''(t)
      θ'(t+1) = θ'(t) + 0.02 * θ''(t)

      We also use Euler's method to estimate x and θ at time t+1 given x, θ, x' and θ' at time t:

      x(t+1) = x(t) + 0.02 * x'(t)
      θ(t+1) = θ(t) + 0.02 * θ'(t)

      This gives us the new state.
      The reward is simple: 0 if non-terminal, -1 if terminal.
         */

        val g   = -9.8     // m/s^2, acceleration due to gravity
        val m_c = 1.0      // kg, mass of cart
        val m   = 0.1      // kg, mass of pole
        val l   = 0.5      // m, half-pole length
        val μ_c = 0.0005   // coefficient of friction of cart on track
        val μ_p = 0.000002 // coefficient of friction of pole on cart
        val F = actionTaken match {
          case PushCart.Left =>
            -10.0 // Newtons, force applied to cart's centre of mass
          case PushCart.Right => 10.0
        }

        val x_t    = currentState.cartPosition
        val `x'_t` = currentState.cartVelocity
        val θ_t    = currentState.poleAngle
        val `θ'_t` = currentState.poleVelocity

        val h = 0.02 // seconds, time step

        val `θ''_t` =
          (g * sin(θ_t) + cos(θ_t) * (-F - m * l * `θ'_t` * `θ'_t` * sin(θ_t) + μ_c * signum(
            `x'_t`)) - ((μ_p * `θ'_t`) / (m * l))) /
            (l * (4.0 / 3.0 - (m * cos(θ_t) * cos(θ_t)) / (m_c + m)))

        val `x''_t` =
          (F + m * l * (`θ'_t` * `θ'_t` * sin(θ_t) - `θ''_t` * cos(θ_t)) - μ_c * signum(`x'_t`)) /
            (m_c + m)

        val `x'_t+1` = `x'_t` + h * `x''_t`
        val `θ'_t+1` = `θ'_t` + h * `θ''_t`

        val `x_t+1` = `x_t` + h * `x'_t`
        val `θ_t+1` = `θ_t` + h * `θ'_t`

        val nextState = PoleCartState(
          cartPosition = `x_t+1`,
          cartVelocity = `x'_t+1`,
          poleAngle = `θ_t+1`,
          poleVelocity = `θ'_t+1`
        )
        val reward = if (isTerminal(nextState)) -1.0 else 0.0

        (nextState, reward)
      }

      override def isTerminal(state: PoleCartState): Boolean = {
        val absPosition     = Math.abs(state.cartPosition)
        val absAngleDegrees = toDegrees(Math.abs(state.poleAngle))
        absPosition > 2.4 || absAngleDegrees > 12
      }

    }

  sealed trait RoughCartPosition
  object RoughCartPosition {
    case object Left   extends RoughCartPosition // x ≦ -0.8 (metres)
    case object Middle extends RoughCartPosition // -0.8 < x ≦ 0.8
    case object Right  extends RoughCartPosition // x > 0.8
  }

  sealed trait RoughCartVelocity
  object RoughCartVelocity {
    case object FastLeft  extends RoughCartVelocity // -0.5 ≦ x (m/s)
    case object Slow      extends RoughCartVelocity // -0.5 < x ≦ 0.5
    case object FastRight extends RoughCartVelocity // x > 0.5
  }

  sealed trait RoughPoleAngle
  object RoughPoleAngle {
    case object VeryLeft      extends RoughPoleAngle // x ≦ -6 (degrees)
    case object QuiteLeft     extends RoughPoleAngle // -6 < x ≦ -1
    case object SlightlyLeft  extends RoughPoleAngle // -1 < x ≦ 0
    case object SlightlyRight extends RoughPoleAngle // 0 < x ≦ 1
    case object QuiteRight    extends RoughPoleAngle // 1 < x ≦ 6
    case object VeryRight     extends RoughPoleAngle // x > 6
  }

  sealed trait RoughPoleVelocity
  object RoughPoleVelocity {
    case object FastLeft  extends RoughPoleVelocity // -50 ≦ x (degrees/second)
    case object Slow      extends RoughPoleVelocity // -50 < x ≦ 50
    case object FastRight extends RoughPoleVelocity // x > 50
  }

  case class RoughPoleCartState(
      cartPosition: RoughCartPosition,
      cartVelocity: RoughCartVelocity,
      poleAngle: RoughPoleAngle,
      poleVelocity: RoughPoleVelocity
  )

  implicit val stateConversion: StateConversion[PoleCartState, RoughPoleCartState] = {
    envState: PoleCartState =>
      val roughCartPosition = envState.cartPosition match {
        case x if x <= -0.8            => RoughCartPosition.Left
        case x if x > -0.8 && x <= 0.8 => RoughCartPosition.Middle
        case _                         => RoughCartPosition.Right
      }

      val roughCartVelocity = envState.cartVelocity match {
        case x if x <= -0.5            => RoughCartVelocity.FastLeft
        case x if x > -0.5 && x <= 0.5 => RoughCartVelocity.Slow
        case _                         => RoughCartVelocity.FastRight
      }

      val roughPoleAngle = toDegrees(envState.poleAngle) match {
        case x if x <= -6.0             => RoughPoleAngle.VeryLeft
        case x if x > -6.0 && x <= -1.0 => RoughPoleAngle.QuiteLeft
        case x if x > -1.0 && x <= 0.0  => RoughPoleAngle.SlightlyLeft
        case x if x > 0.0 && x <= 1.0   => RoughPoleAngle.SlightlyRight
        case x if x > 1.0 && x <= 6.0   => RoughPoleAngle.QuiteRight
        case _                          => RoughPoleAngle.VeryRight
      }

      val roughPoleVelocity = toDegrees(envState.poleVelocity) match {
        case x if x <= -50.0             => RoughPoleVelocity.FastLeft
        case x if x > -50.0 && x <= 50.0 => RoughPoleVelocity.Slow
        case _                           => RoughPoleVelocity.FastRight
      }

      RoughPoleCartState(
        roughCartPosition,
        roughCartVelocity,
        roughPoleAngle,
        roughPoleVelocity
      )
  }

}
