package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {
  
  // assignment ::= identifier ~ "=" ~ expression
def assignment: Parser[Assignment] =identifier~"="~expression ^^ 
{case id~"="~expr => Assignment(id,expr)}  
  
  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
def iteration: Parser[Iteration] ="while"~"("~expression~")"~expression ^^ 
{case "while"~"("~condition~")"~expression => Iteration(condition,expression)}  


  // dereference ::= "[" ~ expression ~ "]"
def dereference: Parser[FunCall] = "["~expression~"]" ^^ 
{case "["~expr~"]" => FunCall(Identifier("dereference"),List(expr))}  

  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | funCall | block | assignment | dereference | literal | "("~>expression<~")"
}