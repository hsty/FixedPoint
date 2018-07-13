import math.abs

object FixedPoint extends App{

  val tolerance: Double  = 0.0001
  def isCloseEnough(x: Double, y: Double) =
    abs((x-y)/x)/x < tolerance

  def fixedPoint(f: Double=> Double): (Double) => Double = {
    def newer(firstGuess: Double): Double = {
      def iterate(guess: Double): Double = {
        //println(guess)
        val next = f(guess)
        if (isCloseEnough(guess, next)) next
        else iterate(next)
      }

      iterate(firstGuess)
    }
    newer
  }

  def averageDamp(f: Double => Double): (Double) => Double = {
    def abc(x: Double): Double = (x + f(x)) / 2
    abc
  }

  //def temp1(x: Double) = x/y

  //def sum(f: Int=> Int)(a: Int, b: Int):Int =
  //  if(a>b) 0 else f(a) + sum(f)(a+1,b)

  //def product(f: Int=>Int)(a: Int, b: Int): Int =
  // if(a>b) 1 else f(a)*product(f)(a+1,b)

  println(product(x=>x)(1,5))

  def fact(x: Int) = product(x=>x)(1,x)

  println(fact(4))

  def combine(f: Int=>Int, g: (Int,Int)=>Int, unit: Int)(a:Int, b:Int):Int =
    if(a>b) unit else g(f(a),combine(f,g,unit)(a+1,b))

  def sum(f: Int=>Int)(a: Int, b: Int) = combine(f, (x:Int, y:Int) => x+y, 0)(a,b)
  def product(f: Int=>Int)(a: Int, b: Int) = combine(f, (x:Int, y:Int) => x*y, 1)(a,b)
  //def sqrt(x: Double) = fixedPoint(y => x/y)(1.0)

  def sqrt(z: Double) = fixedPoint(averageDamp(x => z/x))(1.0)

  // 9:40
  //println(fixedPoint(y => (y + 4/y)/2)(1.0))

  println(sqrt(4))
}
