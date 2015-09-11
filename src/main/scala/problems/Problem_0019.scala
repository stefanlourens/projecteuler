package problems

/**
 * http://projecteuler.net/problem=18
 *
 * How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
 */
object Problem_0019 extends Problem {

  val years = 1901 to 2000
  val months = 1 to 12
  //1901 stars on a tue
  val daysOfWeek = 1 to 7

  def getNumDays(year: Int, month: Int): Int = {
    val isLeap =
      if (year % 1000 == 0) year % 400 == 0
      else year % 4 == 0

    month match {
      case 2 =>
        if (isLeap) 29
        else 28
      case 4 | 5 | 9 | 10 => 30
      case _ => 31
    }
  }

  def answer = {
    val days = for {
      year <- years
      month <- months
      day <- 1 to getNumDays(year, month)
    } yield day

    days zip Stream.continually(daysOfWeek).flatten count { case (dayOfMonth, dayOfWeek) =>
      dayOfMonth == 1 && dayOfWeek == 6
    }
  }

}