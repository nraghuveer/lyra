package lyra
import org.scalajs.dom
import org.scalajs.dom.MouseEvent

trait AppMode:
  def onMouseDown(e: dom.MouseEvent): Unit;
  def onMouseUp(e: dom.MouseEvent): Unit;
  def onMouseMove(e: dom.MouseEvent): Unit;

// Stoke and Selection

// Initial mode for any shape
abstract class ShapeCreatorMode(app: App) extends AppMode {
  def newShape(): ModifiableShape

  private var editee: ModifiableShape = newShape()
  override def onMouseDown(e: MouseEvent): Unit = {
    editee = editee.modify(app.clickToPoint(e))
  }

  override def onMouseMove(e: MouseEvent): Unit = {
    editee = editee.modify(app.clickToPoint(e))
  }

  override def onMouseUp(e: MouseEvent): Unit = {
    app.commandController.log(CreateShapeCommand(editee))
    editee = newShape()
  }
}

class StrokeCreateMode(app: App) extends ShapeCreatorMode(app) {
  override def newShape(): ModifiableShape = {
    StrokeShape(List())
  }
}

//class ReactangleCreateMode(app: App) extends ShapeCreatorMode(app) {
//}
