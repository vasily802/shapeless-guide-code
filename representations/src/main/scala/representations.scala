import shapeless._

final case class Employee(
  name    : String,
  number  : Int,
  manager : Boolean
)

final case class IceCream(
  name        : String,
  numCherries : Int,
  inCone      : Boolean
)

sealed trait Light
case object Red extends Light
case object Amber extends Light
case object Green extends Light

object Main extends Demo {
//  type Light = Red.type :+: Amber.type :+: Green.type :+: CNil
  val employee = Employee("Bill", 1, manager = true)
  val iceCream = IceCream("Cornetto", 0, inCone = true)

  val iceCreamGen = Generic[IceCream]
  val employeeGen = Generic[Employee]
  val icRepr = iceCreamGen.to(iceCream)
  val emp = employeeGen.from(icRepr)

  println(employee)
  println(iceCream)
  println(icRepr)
  println(emp)

  val someRepr = "Somestr" :: 123 :: false :: HNil

  val smtE = employeeGen.from(someRepr)
  val smtI = iceCreamGen.from(someRepr)

  println(smtE)
  println(smtI)

  val lightGen = Generic[Light]

  val red = lightGen.to(Red)
  val amber = lightGen.to(Amber)
  val green = lightGen.to(Green)

  println(red)
  println(amber)
  println(green)

}
