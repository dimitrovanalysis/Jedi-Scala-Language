package context
import scala.util.parsing.combinator._
import expression._
import value._

class Jedi1Parsers extends RegexParsers {

def product:Parser[Expression]=term~rep(("*"|"/")~term)^^{
 case (term~other2) =>parseProduct(term,other2)
}
   
def expression: Parser[Expression] = declaration|conditional|disjunction|failure("Invalid Expression")
 
def declaration: Parser[Declaration] = "def"~identifier ~ "="~expression^^{
 case "def"~identifier ~"="~exppression =>Declaration(identifier,exppression)
}
    
   
def inequality:Parser[Expression]=(sum~opt(("<"|">"|"!=") ~sum))^^{
 case sum ~None =>sum
 case sum ~Some("!=" ~ that) =>FunCall(Identifier("unequals"),List(sum,that))
 case sum ~Some("<" ~ that) =>FunCall(Identifier("less"),List(sum,that))
 case sum ~Some(">" ~ that) =>FunCall(Identifier("more"),List(sum,that))
 
}

def equality: Parser[Expression]= inequality ~rep("==" ~> inequality)^^{
 case inequality ~Nil =>inequality
 case inequality ~plus =>FunCall(Identifier("equals"),inequality::plus)
}

 
def disjunction:Parser[Expression] =conjunction~rep("||" ~>conjunction)^^{
 case conjunct ~Nil =>conjunct
 case conjunct ~plus =>Disjunction(conjunct::plus)
}
   

def conditional:Parser[Conditional] ="if"~"("~expression~")"~expression~opt("else"~expression)^^{
 case "if"~"("~first~")"~secondOpt~None =>Conditional(first,secondOpt)
 case "if"~"("~first~")"~secondOpt~Some("else"~thirdOpt) =>Conditional(first,secondOpt,thirdOpt)
}

def conjunction:Parser[Expression] =equality~rep("&&" ~>equality)^^{
 case equality ~Nil => equality
 case equality ~plus => Conjunction(equality::plus)
}
  

private def negate(exp: Expression): Expression = {
 val sub =Identifier("sub")
 val zero =Integer(0)
 FunCall(sub,List(zero,exp))
}

  
private def parseProduct(exp:Expression,terms:List[~[String,Expression]]):Expression={
  terms match {
   case Nil=>exp
   case ~ ("*",term1)::more=>parseProduct(FunCall(Identifier("mul"),List(exp,term1)),more)
   case ~ ("/",term1)::more=>parseProduct(FunCall(Identifier("div"),List(exp,term1)),more)
  }
}
      
def term:Parser[Expression]=funCall|literal|"("~>expression<~")"
 
def sum:Parser[Expression]=product~rep(("+"|"-")~product^^{
 case "+" ~other1 =>other1
 case "-" ~other1 =>negate(other1)}
)^^
{
 case prod ~Nil =>prod
 case prod ~rest =>FunCall(Identifier("add"),prod::rest)
}
     
def literal=boole|chars|real|integer|identifier

def operands:Parser[List[Expression]] ="("~>opt(expression~rep(","~>expression))<~")"^^{
case None => Nil
case Some(x ~Nil) =>List(x)
case Some(y ~plusSome) =>y::plusSome
}
 
   
def chars:Parser[Chars]="""\"[^"]+\"""".r^^{
   case char =>Chars(char.substring(1,char.length-1))
}
def integer:Parser[Integer]="""0|(\+|-)?[1-9][0-9]*""".r^^{
   case digit =>Integer(digit.toInt) 
}

def identifier:Parser[Identifier]="""[a-zA-Z][a-zA-Z0-9]*""".r^^{
case index =>Identifier(index)
}

def funCall:Parser[FunCall]=identifier~operands^^{
 case operand~operands=>FunCall(operand,operands)
}

def real:Parser[Real]="""(\+|-)?[0-9]+\.[0-9]+""".r^^{
  case reg =>Real(reg.toDouble)
}
def boole:Parser[Boole]=("true"|"false")^^{
 case bool =>Boole(bool.toBoolean)
}

}