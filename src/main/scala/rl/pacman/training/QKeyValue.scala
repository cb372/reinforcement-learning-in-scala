package rl.pacman.training

import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import rl.pacman.core.PacmanProblem.{AgentState, Move}

/*
 This is just an artifact of the way we encode the Q-values as JSON.
 Q is a Map[AgentState, Map[Move, Double]], so it has non-String keys.
 When we write it to the JSON file we turn it into a List[(AgentState, Map[Move, Double])].
 */
case class QKeyValue(key: AgentState, value: Map[Move, Double])

object QKeyValue {

  implicit val moveEncoder: KeyEncoder[Move] = (move: Move) => move.toString
  implicit val moveDecoder: KeyDecoder[Move] = {
    case "Left"  => Some(Move.Left)
    case "Right" => Some(Move.Right)
    case "Up"    => Some(Move.Up)
    case "Down"  => Some(Move.Down)
    case _       => None
  }

  implicit val encoder: Encoder[QKeyValue] = deriveEncoder
  implicit val decoder: Decoder[QKeyValue] = deriveDecoder

}
