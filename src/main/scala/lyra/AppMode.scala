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
  def createShape(initial: Point): ModifiableShape;
  override def onMouseDown(e: MouseEvent): Unit = {
    app.editeeSetter(_ => Some(createShape(app.clickToPoint(e))))
  }

  override def onMouseMove(e: MouseEvent): Unit = {
    app.editeeSetter {
      case Some(shape) => Some(shape.modify(app.clickToPoint(e)))
      case _ => None
    }
  }

  override def onMouseUp(e: MouseEvent): Unit = {
    app.editeeSetter {
      case Some(shape) =>
        app.dataSetter(old => old ++ List[Shape](shape))
        None // add to data and set the current to null
      case _ => None
    }
  }
}

class StrokeCreateMode(app: App) extends ShapeCreatorMode(app) {
  override def createShape(initial: Point): ModifiableShape = {
    StrokeShape(List(initial))
  }
}

//class ReactangleCreateMode(app: App) extends ShapeCreatorMode(app) {
//}
