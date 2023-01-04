package lyra

import org.scalajs.dom

import java.util.Optional

case class Point(x: Double, y: Double) {
  def move(d: Delta): Point = Point(x + d.dx, y + d.dy)
}
case class Rectangle(x: Double, y: Double, w: Double, h: Double) {
  def contains(p: Point): Boolean = {
    val horizontal = p.x >= x && p.x <= x + w
    val vertical = p.y >= y && p.y <= y + h
    horizontal && vertical
  }
}
case class Delta(dx: Double, dy: Double)
object Delta {
  def from(p1: Point, p2: Point): Delta = new Delta(p2.x - p1.x, p2.y - p1.y)
}

trait Shape:
  def overlap(r: Rectangle): Boolean
  def draw(gfx: dom.CanvasRenderingContext2D): Unit
  def move(d: Delta): ModifiableShape

trait ModifiableShape extends Shape {
  def modify(p: Point): ModifiableShape
}

case class StrokeShape(private val contents: List[Point]) extends ModifiableShape {
  def overlap(r: Rectangle): Boolean = {
    // check if each point in the contents is contained in the rectangle
    contents.forall(r.contains)
  }
  def draw(gfx: dom.CanvasRenderingContext2D): Unit = {
    gfx.beginPath()
    for (p <- contents) {
      gfx.lineTo(p.x, p.y)
    }
    gfx.stroke()
  }

  def move(d: Delta): StrokeShape = {
    // move each point with delta
    StrokeShape(contents.map(p => p.move(d)))
  }

  def modify(newPoint: Point): StrokeShape = {
    StrokeShape(this.contents :+ newPoint)
  }
}

