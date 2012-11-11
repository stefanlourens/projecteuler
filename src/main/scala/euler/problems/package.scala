package euler

package object problems {
  
  def ??? : Nothing = throw new Error("an implementation is missing")
  type ??? = Nothing
  type *** = Any

  trait Problem {
    def answer: Any
    def main(args: Array[String]) =
      println(time(answer))
  }

  class TimedResult(val result: Any, val duration: BigInt) {
    override def toString() =
      Console.GREEN + "Answer: " + result + "\nTook: " + duration + "ms"

  }

  def time(f: => Any): TimedResult = {
    val start = System.currentTimeMillis();
    val result = f
    new TimedResult(result, System.currentTimeMillis() - start)
  }


}