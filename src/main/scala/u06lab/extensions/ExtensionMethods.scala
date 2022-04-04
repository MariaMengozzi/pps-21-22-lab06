package u06.extensions

case class Point2D(x: Double, y: Double)

extension (p: Point2D)
  def length: Double = Math.sqrt(p.x * p.x + p.y * p.y)
  def isZero: Boolean = p.x == 0.0 && p.y == 0

object Point2DOperators:
  extension (p: Point2D)
    def +(q: Point2D): Point2D = Point2D(p.x + q.x, p.y + q.y)
    def -(q: Point2D): Point2D = Point2D(p.x - q.x, p.y - q.y)
    def *(q: Point2D): Point2D = Point2D(p.x * q.x, p.y * q.y)

@main def main() =
  // using methods, defined separate to implementation
  val p = Point2D(10, 20)
  println(p.length)
  println(p.isZero)

  // importing and using a desired set of additional operators
  import Point2DOperators.*
  println(p + p)
  println(p - p)
  println((p - p).isZero)
  println(p * p)

  // extending and using a library class
  extension [T](l: List[T])
    def second: T = l.tail.head

  println(List(10,20,30).second)