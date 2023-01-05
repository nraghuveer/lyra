package lyra
import org.scalajs.dom
import scala.collection.mutable.Stack

trait CommandController:
  def log(c: ShapeCommand): Unit
  def undo(): Boolean
  def redo(): Boolean

  def run(setData: DataSetter): Unit

class UndoCommandController extends CommandController {
  private var changes: List[ShapeCommand] = List[ShapeCommand]()
  private val undoStack: Stack[ShapeCommand] = Stack()
  private val redoStack: Stack[UndoCommand] = Stack()

  override def run(setData: DataSetter): Unit = {
    for (cmd <- changes) {
      cmd.run(setData)
    }
  }
  override def log(c: ShapeCommand): Unit = {
    changes = changes ++ List(c)
    // add to undo stack only if it is not undo command, else we endup in loop
    // where only last shape will be undoed
    c match {
      case uc: UndoCommand => redoStack.push(uc)
      case _: ShapeCommand => undoStack.push(c)
    }
  }

  override def undo(): Boolean = {
    if (undoStack.isEmpty) { return false }
    log(new UndoCommand(undoStack.pop()))
    true
  }

  override def redo(): Boolean = {
    if (redoStack.isEmpty) {return false}
    log(new RedoCommand(redoStack.pop()))
    true
  }
}

