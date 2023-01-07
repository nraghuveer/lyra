package lyra

import org.scalajs.dom

import java.util.Optional
import org.scalajs.dom.HTMLSelectElement
import org.scalajs.dom.HTMLButtonElement
import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._


case class StylesConfig(
    color: String,
    lineWidth: Double,
    selectionColor: String,
    selectionPointColor: String,
    opacity: Double
)

class App(canvas: dom.HTMLCanvasElement, initialData: List[Shape]) {
  var styles: StylesConfig = StylesConfig(
    color = "white",
    lineWidth = 4.0,
    selectionColor = "blue",
    selectionPointColor = "red",
    opacity = 1.0
  )
  val user: String = "raghuveer1"

  private var data: List[Shape] = initialData
  private var strokeMode = new StrokeCreateMode(this)
  private var rectangleSelectionMode = new RectangleSelectionMode(this)
  private var mode: AppMode = new StrokeCreateMode(this)
  val commandController = new UndoRedoCommandController()

  switchToEditMode
  attachKeyBindings
  attachResizeBindings
  modeSwitchBindings
  undoRedoBindings

  def switchToEditMode = {
    mode = new StrokeCreateMode(this)
    attachMouseEvtHandlers(mode)
  }
  def undoRedoBindings: Unit = {
    dom.document
      .querySelector("#btnUndo")
      .asInstanceOf[HTMLButtonElement]
      .onclick = (e) => {
      commandController.undo()
      // TODO: Find a fix for this, remove explicit paint call
      paint()
    }

    dom.document
      .querySelector("#btnRedo")
      .asInstanceOf[HTMLButtonElement]
      .onclick = (e) => {
      commandController.redo()
      paint()
    }

  }
  def attachResizeBindings: Unit = {
    def setCanvasSize: Unit = {
      canvas.height = (dom.window.innerHeight - 100).asInstanceOf[Int]
      canvas.width = (dom.window.innerWidth - 100).asInstanceOf[Int]
    }
    dom.window.onresize = (_) => {
      setCanvasSize
    }
    setCanvasSize
  }

  def modeSwitchBindings: Unit = {
    dom.document
      .querySelector("#appModeSelect")
      .asInstanceOf[dom.HTMLInputElement]
      .onchange = (e) => {
      val value = e.target.valueOf
        .asInstanceOf[HTMLSelectElement]
        .value
      value match {
        case "selection" => switchToSelectionMode
        case "edit"      => switchToEditMode
      }
    }
  }

  def attachKeyBindings: Unit = {
    dom.document.onkeydown = renderWrapper((e: dom.KeyboardEvent) => {
      if (e.keyCode == 90 && e.ctrlKey) {
        val ret = commandController.undo()
        println("undo => " + ret)
      } else if (e.keyCode == 89 && e.ctrlKey) {
        val ret = commandController.redo()
        println("redo => " + ret)
      } else if (e.keyCode == 83 && e.ctrlKey) {
        switchToSelectionMode
      } else if (e.keyCode == 69 && e.ctrlKey) {
        switchToEditMode
      }
    })
  }

  def switchToSelectionMode = {
    mode = new RectangleSelectionMode(this)
    attachMouseEvtHandlers(mode)
  }

  // should return a function that takes a event and return unit
  type ActionFnType[E] = E => Unit
  def renderWrapper[E](fn: ActionFnType[E]): ActionFnType[E] = {
    // the return should be executed after then fn is executed
    return (e) => {
      fn(e)
      dom.window.setTimeout(
        () => paint(),
        0
      )
    }
  }

  def attachMouseEvtHandlers(mode: AppMode) = {
    canvas.onmouseup = renderWrapper(e => mode.onMouseUp(e))
    canvas.onmousedown = renderWrapper(e => mode.onMouseDown(e))
    canvas.onmousemove = renderWrapper(e => mode.onMouseMove(e))
    canvas.addEventListener("touchstart", e => mode.onMouseDown(e))
    canvas.addEventListener("touchmove", e => mode.onMouseMove(e))
    canvas.addEventListener("touchend", e => mode.onMouseUp(e))
    canvas.addEventListener("touchleave", e => mode.onMouseUp(e))
  }

  private def clearCanvas(canvas: dom.HTMLCanvasElement): Unit = {
    val gfx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val rect = canvas.getBoundingClientRect()
    gfx.clearRect(rect.x, rect.y, rect.width, rect.height)
  }

  private def paint(): Unit = {
    clearCanvas(canvas)
    data = List()
    commandController.run(this.dataSetter)

    for (shape <- data) {
      shape.draw(canvas)
    }
    // draw if there is a editee from the active mode
    for (shape <- mode.editees) {
      shape.draw(canvas)
    }
  }

  def dataSetter(changeFn: List[Shape] => List[Shape]): Unit = {
    data = changeFn(data)
  }

  def clickToPoint(e: dom.MouseEvent): Point = {
    val boundingRec = canvas.getBoundingClientRect()
    val x = e.clientX - boundingRec.left
    val y = e.clientY - boundingRec.top
    Point(x, y)
  }

  def shapes: List[Shape] = data
}

implicit val stylesConfigDecoder: Decoder[StylesConfig] = deriveDecoder
implicit val stylesConfigEncoder: Encoder[StylesConfig] = deriveEncoder