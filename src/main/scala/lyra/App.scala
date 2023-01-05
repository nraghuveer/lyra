package lyra

import org.scalajs.dom

import java.util.Optional

class App(canvas: dom.HTMLCanvasElement, initialData: List[Shape]) {
  private var data: List[Shape] = initialData
  private val mode: AppMode = new StrokeCreateMode(this)
  val commandController = new UndoCommandController()
  // should return a function that takes a event and return unit
  type MouseActionFnType = dom.MouseEvent => Unit
  def mouseActionWrapper(fn: MouseActionFnType): MouseActionFnType = {
    // the return should be executed after then fn is executed
    return (e) => {
      fn(e)
      dom.window.setTimeout(
        () =>
          paint(
            canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
          ),
        0
      )
    }
  }

  canvas.onmouseup = mouseActionWrapper(e => mode.onMouseUp(e))
  canvas.onmousedown = mouseActionWrapper(e => mode.onMouseDown(e))
  canvas.onmousemove = mouseActionWrapper(e => mode.onMouseMove(e))

  private def paint(gfx: dom.CanvasRenderingContext2D): Unit = {
    val rect = canvas.getBoundingClientRect()
    gfx.clearRect(rect.x, rect.y, rect.width, rect.height)
    gfx.strokeStyle = "white"
    data = List()
    commandController.run(this.dataSetter)
    // draw if there is a editee from the active mode
    mode.curEditee match {
      case Some(editee) => editee.draw(gfx)
      case None         =>
    }
    for (shape <- data) {
      shape.draw(gfx)
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
}
