package doodle.core

import doodle.backend.Canvas

sealed trait Image extends Product with Serializable {
  def draw(canvas: Canvas, originX: Double = 0, originY: Double = 0): Unit = {
    this match {
      case Rectangle(w, h) =>
        canvas.rectangle(originX, originY, w, h)
        canvas.setStroke(Stroke(3.0, Color.black, Line.Cap.Round, Line.Join.Round))
        canvas.stroke()
      case Circle(r)    =>
        canvas.circle(originX, originY, r)
        canvas.setStroke(Stroke(3.0, Color.black, Line.Cap.Round, Line.Join.Round))
        canvas.stroke()
      case On(a, b) =>
        b.draw(canvas)
        a.draw(canvas)
      case Above(a, b) =>
        a.draw(canvas, originX, originY)
        val BoundingBox(_, _, _, h) = BoundingBox.fromImage(a)
        b.draw(canvas, originX, originY + h)
    }
  }
}
final case class Rectangle(width: Double, height: Double) extends Image
final case class Circle(radius: Double) extends Image
final case class On(above: Image, below: Image) extends Image
final case class Above(above: Image, below: Image) extends Image

final case class BoundingBox(left: Double, top: Double, right: Double, bottom: Double)

object BoundingBox {
  def fromImage(image: Image): BoundingBox = {
    image match {
      case Rectangle(w, h) => BoundingBox(0, 0, w, h)
      case Circle(r) => BoundingBox(0, 0, r * 2, r * 2)
      case On(a, b) =>
        val BoundingBox(_, _, aw, ah) = BoundingBox.fromImage(a)
        val BoundingBox(_, _, bw, bh) = BoundingBox.fromImage(b)
        BoundingBox(0, 0, Math.max(aw, bw), Math.max(ah, bh))

      case Above(a, b) =>
        val BoundingBox(_, _, aw, ah) = BoundingBox.fromImage(a)
        val BoundingBox(_, _, bw, bh) = BoundingBox.fromImage(b)
        BoundingBox(0, 0, Math.max(aw, bw), ah + bh)
    }
  }
}

