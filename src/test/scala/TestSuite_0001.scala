import org.scalatest.FunSuite
import euler.problems.Problem_0001._

class TestSuite_0001 extends FunSuite {

  test("natural numbers below 10") {
    assert(solveFor(10) === 23)
  }
  
  test("natural numbers below 1000") {
    assert(answer === 233168)
  }
  
}