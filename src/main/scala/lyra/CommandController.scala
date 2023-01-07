package lyra
import org.scalajs.dom

import scala.collection.mutable

trait CommandController:
  def log(c: ShapeCommand): Unit
  def undo(): Boolean
  def redo(): Boolean
  def run(setData: DataSetter): Unit

class UndoCommandController extends CommandController {
  private var changes: List[ShapeCommand] = List[ShapeCommand]()
  private val undoStack: mutable.Stack[ShapeCommand] = mutable.Stack()
  private val redoStack: mutable.Stack[UndoCommand] = mutable.Stack()

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
      case rc: RedoCommand => undoStack.push(rc)
      // if there is any other shape otherthan undocommand
      // reset the redostack
      case _: ShapeCommand =>
        undoStack.push(c)
        redoStack.popAll()
    }
  }

  override def undo(): Boolean = {
    if (undoStack.isEmpty) { return false }
    log(UndoCommand(undoStack.pop()))
    true
  }

  override def redo(): Boolean = {
    if (redoStack.isEmpty) { return false }
    log(RedoCommand(redoStack.pop()))
    true
  }
}
