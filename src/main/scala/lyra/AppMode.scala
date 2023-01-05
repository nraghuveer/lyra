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
    val shape = createShape(app.clickToPoint(e))
    app.setCurEditShape(Some(shape))
  }

  override def onMouseMove(e: MouseEvent): Unit = {
    if (app.isCurEdit) {
      app.modifyCurEditShape(app.clickToPoint(e))
    }
  }

  override def onMouseUp(e: MouseEvent): Unit = {
    app.getCurEdit match {
      case Some(curEditShape) =>
        app.dataSetter(old => old ++ List[Shape](curEditShape))
        app.setCurEditShape(None)
      case _ =>
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
