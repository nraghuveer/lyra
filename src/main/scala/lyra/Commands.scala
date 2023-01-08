package lyra

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._


// shapes commands should only take something related to shapes!!!
sealed trait ShapeCommand[T <: Shape[T]]:
  def undo(setData: DataSetter[T]): Unit
  def run(setData: DataSetter[T]): Unit

case class CreateShapeCommand[T <: Shape[T]](shape: T) extends ShapeCommand[T] {
  def run(setData: DataSetter[T]): Unit = {
    setData((old: List[T]) => {
      old ++ List[T](shape)
    })
  }
  def undo(setData: DataSetter[T]): Unit = {
    setData(old => old.filter(s => s != shape))
  }
}

case class MoveShapesCommand[T <: Shape[T]](shapes: List[T], delta: Delta)
    extends ShapeCommand[T] {
  override def run(setData: DataSetter[T]): Unit = {
    // move the shapes if it is part of the given shapes with given delta
    setData(old =>
      old.map(s => {
        if (shapes.contains(s)) {
          s.move(delta)
        } else { s }
      })
    )
  }

  override def undo(setData: DataSetter[T]): Unit = {
    setData(old => {
      old.map(s => {
        // before checking if the shape exists in the list
        // apply the move...
        if (shapes.map(s => s.move(delta)).contains(s)) {
          s.move(delta.reverse())
        } else { s }
      })
    })
  }
}

case class UndoCommand[T <: Shape[T]](command: ShapeCommand[T]) extends ShapeCommand[T] {
  override def run(setData: DataSetter[T]): Unit = {
    command.undo(setData)
  }

  override def undo(setData: DataSetter[T]): Unit = {
    command.run(setData)
  }
}

case class RedoCommand[T <: Shape[T]](command: ShapeCommand[T]) extends ShapeCommand[T] {
  override def run(setData: DataSetter[T]): Unit = {
    command.undo(setData)
  }

  override def undo(setData: DataSetter[T]): Unit = {
    command.run(setData)
  }
}

// diffcommand shuld take a diffableshape, because it doesnt make sense to have diffcommand for non-diffablehsapes
// site variance
//trait DiffCommand[T <: DiffableShape[_ <: DiffableShape[_]]](selfShape: T) extends ShapeCommand:
//  def checkConstraints(other: T): Boolean;



implicit val createShapeCommandDecoder: Decoder[CreateShapeCommand[_]] = deriveDecoder
implicit val createShapeCommandEncoder: Encoder[CreateShapeCommand[_]] = deriveEncoder

//implicit val shapeCommandDecoder: Decoder[ShapeCommand[_]] = deriveDecoder
//implicit val shapeCommandEncoder: Encoder[ShapeCommand[_]] = deriveEncoder
//implicit val undoCommandDecoder: Decoder[UndoCommand[_]] = deriveDecoder
//implicit val undoCommandEncoder: Encoder[UndoCommand[_]] = deriveEncoder
//implicit val redoCommandDecoder: Decoder[RedoCommand[_]] = deriveDecoder
//implicit val redoCommandEncoder: Encoder[RedoCommand[_]] = deriveEncoder
//implicit val diffCommandDecoder: Decoder[DiffCommand[T <: DiffableShape[T]]] = deriveDecoder[DiffCommand[T <: DiffableShape[T]]]
//implicit val diffCommandEncoder: Decoder[DiffCommand[T <: DiffableShape[T]]] = deriveEncoder
