package u04lab.polyglot.a01b
import scala.jdk.javaapi.OptionConverters
import u04lab.polyglot.OptionToOptional
import u04lab.code.Option
import u04lab.code.Option.*
import u04lab.code.List
import u04lab.code.List.*
import u04lab.code.Stream
import u04lab.code.Stream.*
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:

  val r = Random()

  var mineSet: List[(Int, Int)] = Nil()
  var selected: List[(Int, Int)] = Nil()

  while length(mineSet) < mines do
    val point = (r.nextInt(size), r.nextInt(size))
    if !contains(mineSet, point) then
      mineSet = append(mineSet, Cons(point, Nil()))
  println(mineSet)


  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if contains(mineSet, (x, y)) then
      OptionToOptional(None()) // Option => Optional converter
    append(selected, Cons((x, y), Nil()))
    OptionToOptional(neighbours(x, y))

  def neighbours(x: Int, y: Int): Option[Int] =
    //this gives me all neighbour points to the one that was clicked
    val elements = List.flatMap(Stream.toList(Stream.map(Stream.take(Stream.iterate(x - 1)(el => el + 1))(3))(xx =>
      Stream.toList(Stream.map(Stream.take(Stream.iterate(y - 1)(el => el + 1))(3))(yy => (xx, yy))))))(l => append(Nil(), l))
    Some(List.length(List.filter(elements)(el => contains(mineSet, el))))
    //println(List.flatMap(elements)(el => el match
    //  case ms: List[_] => ))
    //Stream.take()


  def won: Boolean = length(selected) + length(mineSet) == size * size
