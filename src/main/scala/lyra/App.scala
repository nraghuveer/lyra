package lyra

import org.scalajs.dom

import java.util.Optional

case class StylesConfig(
    color: String,
    lineWidth: Double,
    selectionColor: String
)

class App(canvas: dom.HTMLCanvasElement, initialData: List[Shape]) {
  private var data: List[Shape] = initialData
  private val mode: AppMode = new StrokeCreateMode(this)
  val styles = StylesConfig("white", 4.0, "blue")
  val commandController = new UndoCommandController()

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

  canvas.onmouseup = renderWrapper(e => mode.onMouseUp(e))
  canvas.onmousedown = renderWrapper(e => mode.onMouseDown(e))
  canvas.onmousemove = renderWrapper(e => mode.onMouseMove(e))
  dom.document.onkeydown = renderWrapper((e: dom.KeyboardEvent) => {
    if (e.keyCode == 90 && e.ctrlKey) {
      val ret = commandController.undo()
      println("undo => " + ret)
    } else if (e.keyCode == 89 && e.ctrlKey) {
      val ret = commandController.redo()
      println("redo => " + ret)
    }
  })

  private def clearCanvas(canvas: dom.HTMLCanvasElement): Unit = {
    val gfx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val rect = canvas.getBoundingClientRect()
    gfx.clearRect(rect.x, rect.y, rect.width, rect.height)
  }

  private def paint(): Unit = {
    clearCanvas(canvas)
    data = List()
    commandController.run(this.dataSetter)
    // draw if there is a editee from the active mode
    for (shape <- mode.editees) {
      shape.draw(canvas)
    }
    for (shape <- data) {
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
