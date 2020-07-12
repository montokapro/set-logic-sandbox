package potree

import scala.{specialized => sp}
trait Inverse[@sp(Int, Long, Float, Double) A] extends Any {
  def inverse(a: A): A
}
