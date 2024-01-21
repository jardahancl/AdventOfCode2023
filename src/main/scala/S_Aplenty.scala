import scala.io.Source


object S_Aplenty {
	case class Rating(x: Int, m: Int, a: Int, s: Int)
	
	case class Result(satisfied: Boolean, next: String)
	
	case class Workflow(name: String, funs: List[Rating => Result])
	
	def solution() = {
		val startTime = System.currentTimeMillis()
		val input = Source.fromFile("src/main/resource/19_test.txt").getLines.toList
		val workflows = extractWorkflows(input)
		val workflowStrings = extractWorkflowStrings(input)
		val ratings = extractRatings(input)
		
		val res1 = ratings.map(rating => evaluateRating(workflows, rating, 1)).sum
		println(s"Solution 12a: $res1")
		
		val res2 = part2(workflows, workflowStrings)
		println(s"Solution 12b: $res2")
		
		println(s"Duration ${(System.currentTimeMillis() - startTime) / 1000} seconds")
	}
	
	case class Condition(par: String, op: String, value: Int)
	
	case class GroupRatings(caseValue: Int, size: Long)
	
	def part2(workflows: List[Workflow], workflowStrings: List[String]) = {		
		val res = for {
			xList <- getWorkflowList("x", workflowStrings)
			mList <- getWorkflowList("m", workflowStrings)
			aList <- getWorkflowList("a", workflowStrings)
			sList <- getWorkflowList("s", workflowStrings)
		} yield evaluateRating(workflows, Rating(xList.caseValue, mList.caseValue, aList.caseValue, sList.caseValue), 2) * xList.size * mList.size * aList.size * sList.size
		
		res.sum
	}
	
	def getWorkflowList(parameter: String, workflowStrings: List[String]): List[GroupRatings] = {
		val upperValues = workflowStrings.filter(str => str.startsWith(parameter))
			.map(str => Condition(str.charAt(0).toString, str.charAt(1).toString, str.substring(2).toInt))
			.map(c => c match
				case Condition(_, ">", i) => i
				case Condition(_, "<", i) => i - 1
			)
		val upperValuesSorted = (upperValues ++ List(0, 4000)).sorted
		val groupRatings = upperValuesSorted.zip(upperValuesSorted.tail)
			.map((low, high) => GroupRatings(high, high - low))
			.filter(gr => gr.size != 0)
		
		groupRatings
	}
	
	def evaluateRating(workflows: List[Workflow], rating: Rating, part: Int) = {
		var currentName = "in"
		while (!List("A", "R").contains(currentName)) {
			val currentWorkflow: Workflow = workflows.filter(w => w.name == currentName).head
			currentName = currentWorkflow.funs.map(f => f(rating))
				.filter(res => res.satisfied)
				.head.next
		}
		
		val evaluation = (part, currentName) match
			case (2, "A") => 1
			case (1, "A") => rating.x + rating.m + rating.a + rating.s
			case (_, _) => 0
		
		evaluation
	}
	
	
	def extractRatings(input: List[String]) = {
		input.filter(line => line.nonEmpty)
			.filter(line => line.startsWith("{"))
			.map(line => {
				val splitted = line.substring(1, line.length - 1).split(",")
				Rating(
					splitted(0).split("=")(1).toInt,
					splitted(1).split("=")(1).toInt,
					splitted(2).split("=")(1).toInt,
					splitted(3).split("=")(1).toInt
				)
			})
	}
	
	def extractWorkflows(input: List[String]) = {
		input.filter(line => line.nonEmpty)
			.filter(line => !line.startsWith("{"))
			.map(line => {
				val name = line.substring(0, line.indexOf('{'))
				val conditions = line.substring(line.indexOf('{') + 1, line.indexOf('}')).split(",").toList
					.map(condInString => createCondition(condInString))
				
				Workflow(name, conditions)
			})
	}
	
	def extractWorkflowStrings(input: List[String]) = {
		input.filter(line => line.nonEmpty)
			.filter(line => !line.startsWith("{"))
			.flatMap(line => {
				line.substring(line.indexOf('{') + 1, line.indexOf('}'))
					.split(",").toList
					.filter(str => str.contains(':'))
					.map(str => str.split(":")(0))
			})
	}
	
	def createCondition(inputString: String)(rating: Rating): Result = {
		if (inputString.contains(":")) {
			val splitted = inputString.split(":")
			val parameter = splitted(0).charAt(0) match
				case 'x' => rating.x
				case 'm' => rating.m
				case 'a' => rating.a
				case 's' => rating.s
			val value = splitted(0).substring(2).toInt
			val isSatisfied = splitted(0).charAt(1) match
				case '<' => parameter < value
				case '>' => parameter > value
			
			Result(isSatisfied, splitted(1))
		}
		else {
			Result(true, inputString)
		}
	}
	
}
