package jsonParser

object OwnJsonObjectParser extends App {

  case class Customer(email: String, name: String)

  case class Item(id: Int, description: String)

  case class Order(customer: Customer, items: List[Item])

  val customer = Customer("alice@mail.com", "Alice")
  val order = Order(customer, List(
    Item(1, "first item"),
    Item(2, "second item"),
  ))

  trait JsonValue {
    def stringify: String
  }

  case class JsonString(value: String) extends JsonValue {
    override def stringify: String = "\"" + value + "\""
  }

  case class JsonInteger(value: Int) extends JsonValue {
    override def stringify: String = value.toString
  }

  case class JsonArray(value: List[JsonValue]) extends JsonValue {
    override def stringify: String = value.map(_.stringify).mkString("[", ",", "]")
  }

  case class JsonObject(value: Map[String, JsonValue]) extends JsonValue {
    override def stringify: String = value.map {
      case (k, v) => "\"" + k + "\":" + v.stringify
    }.mkString("{", ",", "}")
  }

  val data = JsonObject(Map(
    "name" -> JsonString("Bob"),
    "items" -> JsonArray(
      List(
        JsonInteger(1),
        JsonString("Stuff to buy"))
    )
  ))

  println(data.stringify)
  // {"name":"Bob","items":[1,"Stuff to buy"]}

  trait Converter[T] {
    def convert(value: T): JsonValue
  }

  implicit class JsonConverter[T](value: T) {
    def toJSON(implicit converter: Converter[T]): JsonValue =
      converter.convert(value)
  }

  implicit object StringConverter extends Converter[String] {
    def convert(value: String): JsonValue = JsonString(value)
  }

  implicit object IntConverter extends Converter[Int] {
    def convert(value: Int): JsonValue = JsonInteger(value)
  }

  implicit object CustomerConverter extends Converter[Customer] {
    override def convert(value: Customer): JsonValue = JsonObject(Map(
      "email" -> JsonString(value.email),
      "name" -> JsonString(value.name)
    ))
  }

  implicit object ItemConverter extends Converter[Item] {
    override def convert(value: Item): JsonValue = JsonObject(Map(
      "id" -> JsonInteger(value.id),
      "description" -> JsonString(value.description)
    ))
  }

  implicit object OrderConverter extends Converter[Order]{
    override def convert(value: Order): JsonValue = JsonObject(Map(
      "customer" -> order.customer.toJSON,
      "items" -> JsonArray(order.items.map(_.toJSON))
    ))
  }

  println(order.toJSON.stringify)
  //{"customer":{"email":"alice@mail.com","name":"Alice"},"items":[{"id":1,"description":"first item"},{"id":2,"description":"second item"}]}

}
