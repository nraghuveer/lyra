package lyra

import scala.scalajs.js
import org.scalajs.dom

@main
def Lyra(): Unit = {
    val canvasElement = dom.document.querySelector("#appCanvas").asInstanceOf[dom.HTMLCanvasElement]
    val app = App(canvasElement, List[Shape]())
    println("after app")
//    val ctx = canvasElement.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
//    ctx.fillStyle = "black"
//    ctx.fillRect(0,0,255,255.0)
}