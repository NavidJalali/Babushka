package core

sealed trait JsonValue

object JsonValue {
  case object JsonNull extends JsonValue
  case class JsonNumber(number: Double) extends JsonValue
  case class JsonString(string: String) extends JsonValue
  case class JsonBool(boolean: Boolean) extends JsonValue
  case class JsonArray(array: Array[JsonValue]) extends JsonValue
  case class JsonObject(map: Map[String, JsonValue]) extends JsonValue
}
