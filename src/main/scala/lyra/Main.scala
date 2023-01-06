package lyra

import scala.scalajs.js
import org.scalajs.dom

@main
def Lyra(): Unit = {
  val canvasElement =
    dom.document.querySelector("#appCanvas").asInstanceOf[dom.HTMLCanvasElement]
  val _ = App(canvasElement, List[Shape]())
}
