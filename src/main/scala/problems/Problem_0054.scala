package problems

import io.Source

/**
 * http://projecteuler.net/problem=54
 *
 * How many hands does Player 1 win?
 */
object Problem_0054 extends Problem {

  class Card(sig: String) extends Ordered[Card] {
    val suit: Char = sig(1)
    val value = sig(0)
    val numValue = sig(0) match {
      case 'T' => 10
      case 'J' => 11
      case 'Q' => 12
      case 'K' => 13
      case 'A' => 14
      case n => n.asDigit
    }

    override def compare(that: Card): Int = numValue - that.numValue
    override def toString = sig
  }

  class Hand(val cards: List[Card]) {
    lazy val highCard: Card = cards maxBy { _.numValue }

    lazy val onePair: Option[List[Card]] = {
      val pairCards = cards groupBy { _.numValue } filter { case (k, v) => v.length == 2 }
      if (pairCards.size == 1) Some(pairCards map { _._2 } toList (0))
      else None
    }

    lazy val twoPair: Option[List[Card]] = {
      val pairCards = cards groupBy { _.numValue } filter { case (k, v) => v.length == 2 }
      if (pairCards.size == 2) Some(pairCards flatMap { _._2 } toList)
      else None
    }

    lazy val threeOfAKind: Option[List[Card]] = {
      val kindCards = cards groupBy { _.numValue } find { case (k, v) => v.length == 3 }
      if (kindCards.isDefined) Some(kindCards.get._2)
      else None
    }

    lazy val straight: Option[List[Card]] = {
      val sorted = cards.map { _.numValue }.sorted
      if (sorted zip sorted.tail forall { case (a, b) => a + 1 == b }) Some(convertAce(cards))
      else None
    }

    lazy val flush: Option[List[Card]] =
      if (cards.forall { _.suit == cards.head.suit }) Some(cards)
      else None

    lazy val fullHouse: Option[List[Card]] =
      if (threeOfAKind.isDefined && onePair.isDefined) Some(cards)
      else None

    lazy val straightFlush: Option[List[Card]] =
      if (flush.isDefined && straight.isDefined) Some(convertAce(cards))
      else None

    lazy val royalFlush: Option[List[Card]] =
      if (flush.isDefined && cards.map { _.value }.forall { List('T', 'J', 'Q', 'K', 'A').contains }) Some(cards)
      else None

    lazy val (cardsUsed, value): (List[Card], Int) = {
      val hands = List(onePair, twoPair, threeOfAKind, straight, flush, fullHouse, straightFlush, royalFlush)
      val rank = (hands zipWithIndex).reverse.find { case (r, _) => r.isDefined }
      if (rank.isDefined) rank.get match { case (c, v) => (c.get, v) }
      else (List(highCard), -1)
    }

    private def convertAce(cards: List[Card]): List[Card] = {
      val ace = cards find { _.value == 'A' }
      val two = cards find { _.numValue == 2 }

      if (ace.isDefined && two.isDefined) {
        val aceIdx = cards.indexOf(ace)
        cards.updated(aceIdx, new Card("1" + ace.get.suit))
      } else cards
    }

    def >(that: Hand): Boolean = {
      if (value != that.value) value > that.value
      else if (fullHouse.isDefined) threeOfAKind.get.head.numValue > that.threeOfAKind.get.head.numValue
      else {
        val diff = cardsUsed.sorted.reverse zip that.cardsUsed.sorted.reverse find { case (l, r) => l.numValue != r.numValue }
        if (diff.isDefined) diff.get match { case (l, r) => l.numValue > r.numValue }
        else {
          val kicker = cards.sorted.reverse zip that.cards.sorted.reverse find { case (l, r) => l.numValue != r.numValue }
          if (kicker.isDefined) kicker.get match { case (l, r) => l.numValue > r.numValue }
          else {
            throw new Error("Hands are the same")
          }
        }
      }
    }

    def <(that: Hand): Boolean = !(this > that)

    override def toString = {
      val handStr =
        if (royalFlush.isDefined) "Royal Flush"
        else if (straightFlush.isDefined) "Straight Flush"
        else if (fullHouse.isDefined) "Full House"
        else if (flush.isDefined) "Flush"
        else if (straight.isDefined) "Straight"
        else if (threeOfAKind.isDefined) "Three Of A Kind"
        else if (twoPair.isDefined) "Two Pair"
        else if (onePair.isDefined) "One Pair"
        else "High Card"

      (cards.sorted mkString ", ") + s" ($handStr)\t\t\t\t"
    }

  }

  def answer = {
    val deals = Source.fromFile("resources/0054/poker.txt").getLines()
    val hands = deals map {
      row =>
        {
          val (player1, player2) = row.split(" ").splitAt(5)
          (new Hand(player1.map { new Card(_) }.toList), new Hand(player2.map { new Card(_) }.toList))
        }
    }

    hands count { case (hand1, hand2) => hand1 > hand2 }
  }

}