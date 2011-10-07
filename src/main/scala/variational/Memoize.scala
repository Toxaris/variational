package variational

import collection.mutable

/**
 * @author Tillmann Rendel
 */

trait Memoize[A, B] extends (A => B) {
  private[this] val cache : mutable.Map[A, B] = mutable.Map()

  protected def process(v1 : A) : B

  def apply(v1 : A) : B =
    cache.getOrElseUpdate(v1, process(v1))
}