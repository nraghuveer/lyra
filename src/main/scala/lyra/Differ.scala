package lyra

import lyra.DiffSync.{modifiableShapeFilter, shapeListToDict}

import scala.language.postfixOps

trait Differ {
  val copy: List[Shape[_]]
  def diff(current: List[Shape[_]]): List[ShapeCommand]
}

class DiffSync(val copy: List[Shape[_]]) extends Differ {
  override def diff(current: List[_ <: Shape[_]]): List[ShapeCommand] = {
    val mCopy = shapeListToDict { copy.filter(modifiableShapeFilter) }
    val mCur = shapeListToDict { current.filter(modifiableShapeFilter) }
    val zipped = (mCopy.keys ++ mCur.keys).map(key => {
      (key, mCopy.get(key), mCur.get(key))
    })
    zipped.map((_, one, two) => {
      (one, two) match {
        case (Some(_), Some(sCur)) => Some(MoveShapesCommand(List(sCur), Delta(0, 0)))
        case (Some(sCopy), None) => Some(DeleteShapeCommand(sCopy))
        case (None, Some(sCur)) => Some(CreateShapeCommand(sCur))
        case (None, None) => None
      }
    }).toList.flatten
  }
}

object DiffSync {

  def shapeListToDict(shapes: List[Shape[_]]): Map[String, Shape[_]] = {
    shapes.map(shape => (shape.id, shape)).toMap
  }
  def modifiableShapeFilter(x: Shape[_]): Boolean = x match {
    case _: ModifiableShape[_] => true
    case _ => false
  }
}
