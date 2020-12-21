package expression
import context._
import value._

case class Block (val expr:List[Expression]) extends SpecialForm { 
	def execute(environment:Environment):Value = {
	  val tempEnv = new Environment(environment)
	  val mappedExpr = expr.map(_.execute(tempEnv))
	  mappedExpr(mappedExpr.size-1)
	}
}