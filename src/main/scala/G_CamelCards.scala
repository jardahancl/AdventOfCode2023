import scala.io.Source

object G_CamelCards {
	case class Hand(cards: String, bid: Int)
	
	case class CardCount(card: Char, num: Int)
	
	val orderBase = "1123456789TJQKA"
	val orderJoker = "11J23456789TQKA"
	
	def solution() = {
		val input = Source.fromFile("src/main/resource/07_prod.txt").getLines.toList
		val hands = input.map(line => Hand(line.split(" ")(0), line.split(" ")(1).toInt))
		
		val sortedRes1 = hands
			.map(hand => (hand, getMultiplicity(hand.cards)))
			.sortBy((h, m) => getSortFunction(h, m, orderBase))
		val sortedRes2 = hands
			.map(hand => (hand, getMultiplicity(hand.cards)))
			.map(t => (t._1, updateMultiplicity(t._1.cards, t._2)))
			.sortBy((h, m) => getSortFunction(h, m, orderJoker))
		
		println(getResult(sortedRes1))
		println(getResult(sortedRes2))
	}
	
	def getResult(sorted: List[(Hand, List[CardCount])]): Int = {
		sorted.map((h, m) => h)
			.zipWithIndex
			.map((h, i) => h.bid * (i + 1))
			.sum
	}
	
	def getSortFunction(h: Hand, m: List[CardCount], order: String) = {
		(
			if (m.head.num == 5) getScore(h.cards, order) else 0,
			if (m.head.num == 4) getScore(h.cards, order) else 0,
			if (m.head.num == 3 && m(1).num == 2) getScore(h.cards, order) else 0,
			if (m.head.num == 3 && m(1).num == 1) getScore(h.cards, order) else 0,
			if (m.head.num == 2 && m(1).num == 2) getScore(h.cards, order) else 0,
			if (m.head.num == 2 && m(1).num == 1) getScore(h.cards, order) else 0,
			getScore(h.cards, order)
		)
	}
	
	def getScore(c: String, order: String): Int = {
		order.indexOf(c.charAt(0)) * 100_000_000
			+ order.indexOf(c.charAt(1)) * 1_000_000
			+ order.indexOf(c.charAt(2)) * 10_000
			+ order.indexOf(c.charAt(3)) * 100
			+ order.indexOf(c.charAt(4))
	}
	
	def getMultiplicity(s: String): List[CardCount] = {
		val multiplicities = s.distinct
			.map(c => CardCount(c, s.count(_ == c)))
			.sortBy(cc => (cc.num, orderBase.indexOf(cc.card))).reverse
			.toList
		
		multiplicities ++ List.fill(5 - multiplicities.length)(CardCount('1', 0))
	}
	
	def updateMultiplicity(s: String, multiplicities: List[CardCount]) = {
		val updatedChar = if (multiplicities.head.card != 'J') multiplicities.head.card else if (multiplicities.length != 1) multiplicities(1).card else 'A'
		val updatedString = s.replace('J', updatedChar)
		
		val jokerMultiplicities = updatedString.distinct
			.map(c => CardCount(c, updatedString.count(_ == c)))
			.sortBy(cc => (cc.num, orderJoker.indexOf(cc.card))).reverse
			.toList
		
		jokerMultiplicities ++ List.fill(5 - jokerMultiplicities.length)(CardCount('1', 0))
	}
}
