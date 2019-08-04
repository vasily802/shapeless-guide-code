import shapeless._

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  def pure[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] =
        func(value)
    }

  implicit val stringEnc: CsvEncoder[String] =
    pure(str => List(str))

  implicit val intEnc: CsvEncoder[Int] =
    pure(num => List(num.toString))

  implicit val booleanEnc: CsvEncoder[Boolean] =
    pure(bool => List(if(bool) "yes" else "no"))


  implicit val hnilEncoder: CsvEncoder[HNil] =
    pure(_ => Nil)

  implicit def hlistEncoder[H, T <: HList](
                                            implicit
                                            hEncoder: CsvEncoder[H],
                                            tEncoder: CsvEncoder[T]
                                          ): CsvEncoder[H :: T] =
    pure {
      case h :: t =>
        hEncoder.encode(h) ++ tEncoder.encode(t)
    }

  implicit val employeeEncoder: CsvEncoder[Employee] =
    pure(e => List(e.name, e.number.toString, e.manager.toString))
}

case class Employee(name: String, number: Int, manager: Boolean)
case class IceCream(name: String, numCherries: Int, inCone: Boolean)

object Main extends Demo {
  def encodeCsv[A](value: A)(implicit enc: CsvEncoder[A]): List[String] =
    enc.encode(value)

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): Unit =
    println(values.map(value => enc.encode(value).mkString(",")).mkString("\n"))

  println(encodeCsv("Dave"))
  println(encodeCsv(123))
  println(encodeCsv(true))

  val e1 = Employee("Dave",123,true)
  val e2 = Employee("Dave2",123,true)
  println(encodeCsv(e1))

  writeCsv(List(e1,e2))
  println("shheesh")
  println(encodeCsv(Generic[Employee].to(e1)))
}


