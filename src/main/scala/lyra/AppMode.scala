package lyra
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import scala.runtime.Static

trait AppMode:
  def onMouseDown(e: dom.MouseEvent): Unit;
  def onMouseUp(e: dom.MouseEvent): Unit;
  def onMouseMove(e: dom.MouseEvent): Unit;
  def editees: List[StaticShape];
  def clearState: Unit

trait SelectionMode extends AppMode:
  def isSelection: Boolean
  def isOnSelectionContour(p: Point): Boolean
  def isInSelection(point: Option[Point]): Boolean
  def selectedShapes: List[Shape]
  def highlightShapes: List[Shape]

// Stoke and Selection
class RectangleSelectionMode(app: App) extends SelectionMode {
  var selectionRect: Option[Rectangle] = None
  var selectionRectStart: Option[Point] = None
  private val dragMode: AppMode = new RectangleSelectionDragMode(app, this)

  override def clearState: Unit = {
    selectionRect = None
    selectionRectStart = None
  }
  override def isOnSelectionContour(p: Point): Boolean = selectionRect match {
    case Some(rect) => rect.onContour(p)
    case None       => false
  }
  override def isSelection: Boolean = selectionRect match {
    case Some(_) => true
    case None    => false
  }

  def isOnSelection(point: Option[Point]): Boolean = {
    point match {
      case Some(p) =>
        selectionRect match {
          case Some(rect) => rect.contains(p)
          case None       => false
        }
      case None => false
    }
  }

  override def isInSelection(point: Option[Point]): Boolean = {
    point match {
      case Some(p) =>
        selectionRect match {
          case Some(rect) => rect.contains(p)
          case None       => false
        }
      case None => false
    }
  }

  override def selectedShapes: List[Shape] = {
    // this should have different styles
    // the idea is to highlight the start and endpoints....
    // if bounding rectangle
    selectionRect match {
      case Some(rect) =>
        app.shapes
          .filter(shape => shape.overlap(rect))
      case None => List()
    }
  }

  override def highlightShapes: List[Shape] = {
    selectionRect match {
      case Some(rect) =>
        selectedShapes
          .map(shape => EndpointsHightlight(shape.highlights, app.styles))
      case None => List()
    }
  }

  def editees: List[StaticShape] =
    // selection Rectangle ++ shapes bounded in this rectangle
    val rect = selectionRect match {
      case Some(rect) =>
        List(SelectionRectShape(rect, app.styles.copy(lineWidth = 2)))
      case None => List()
    }
    rect ++ highlightShapes ++ dragMode.editees

  def onMouseDown(e: MouseEvent): Unit = {
    val p = app.clickToPoint(e)
    // clear only if the point is not IN selection
    if (!isInSelection(Some(p))) {
      selectionRectStart = Some(app.clickToPoint(e))
      selectionRect = None
    }
    dragMode.onMouseDown(e)
  }
  def onMouseUp(e: MouseEvent): Unit = {
    // support only top to bottom
    val p = app.clickToPoint(e)
    selectionRectStart match {
      case Some(start) =>
        val w = p.x - start.x
        val h = p.y - start.y
        selectionRect = Some(Rectangle(start.x, start.y, w, h))
      case None =>
    }
    selectionRectStart = None
    dragMode.onMouseUp(e)
  }
  def onMouseMove(e: MouseEvent): Unit = {
    val p = app.clickToPoint(e)
    selectionRectStart match {
      case Some(start) =>
        val w = p.x - start.x
        val h = p.y - start.y
        selectionRect = Some(Rectangle(start.x, start.y, w, h))
      case None =>
    }
    dragMode.onMouseMove(e)
  }
}

class RectangleSelectionDragMode(app: App, selectionMode: SelectionMode)
    extends AppMode {
  private val opacity: Double = 0.3
  private var start: Option[Point] = None
  private var dragDelta: Option[Delta] = None

  override def clearState: Unit = {
    start = None
    dragDelta = None
  }
  override def onMouseDown(e: MouseEvent): Unit = {
    val p = app.clickToPoint(e)
    if (selectionMode.isInSelection(Some(p))) {
      start = Some(p)
    } else {
      start = None
    }
  }
  override def onMouseMove(e: MouseEvent): Unit = {
    val dragEnd = app.clickToPoint(e)
    start match {
      case Some(dragStart) =>
        dragDelta = Some(Delta.from(dragStart, dragEnd))
      case None =>
        dragDelta = None
    }
  }

  override def onMouseUp(e: MouseEvent): Unit = {
    dragDelta match {
      case Some(delta) =>
        val cmd =
          MoveShapesCommand(selectionMode.selectedShapes, delta)
        app.commandController.log(cmd)
        clearState
      case None =>
    }
    clearState

  }

  override def editees: List[StaticShape] = {
    dragDelta match {
      case Some(delta) =>
        (selectionMode.selectedShapes)
          .map(shape => shape.move(delta))
          .map(shape => shape.patchStyles(shape.styles.copy(opacity = opacity)))
      case None => List()
    }
  }
}

// Initial mode for any shape
abstract class CreationMode(app: App) extends AppMode {
  def newShape(): ModifiableShape

  private var editee: Option[ModifiableShape] = None

  override def editees: List[StaticShape] = editee match {
    case Some(shape) => List(shape.asInstanceOf[StaticShape])
    case None        => List()
  }

  override def onMouseDown(e: MouseEvent): Unit = {
    editee = Some(newShape().modify(app.clickToPoint(e)))
  }

  override def onMouseMove(e: MouseEvent): Unit = {
    editee = editee match {
      case Some(editShape) => Some(editShape.modify(app.clickToPoint(e)))
      case None            => None
    }
  }

  override def onMouseUp(e: MouseEvent): Unit = {
    editee = editee match {
      case Some(editShape) =>
        app.commandController.log(CreateShapeCommand(editShape))
        None
      case None => None
    }
  }
}

class StrokeCreateMode(app: App) extends CreationMode(app) {
  override def clearState: Unit = {}
  override def newShape(): ModifiableShape = {
    StrokeShape(List(), app.styles)
  }
}
