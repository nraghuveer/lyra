package lyra

import scala.language.postfixOps


trait Differ[T <: Shape]:
  val oldData: List[T]
  val newData: List[T]
  def generateDiff(): List[T]

class CommandDiffer[T <: DiffableShape[ModifiableShape]](val oldData: List[T],
                           val newData: List[T]) extends Differ[T] {
  override def generateDiff(): List[T] = {
    oldData.map {
      case x: StrokeShape => println(x.diff(x)); ""
      case _ => ""
    }
    List()
  }
}



