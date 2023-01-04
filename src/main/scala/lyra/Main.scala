package lyra

import scala.scalajs.js
import org.scalajs.dom

@main
def Lyra(): Unit = {
    dom.document.querySelector("#app").innerHTML = """
        <h1>Hello scala.js & Vite</h1>
    """
}