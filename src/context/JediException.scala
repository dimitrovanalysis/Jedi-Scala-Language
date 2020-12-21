package context
import scala.util.parsing.combinator._
import expression.Identifier
//from 1.0
class JediException(val gripe: String = "Jedi error ") extends Exception(gripe) 
class UndefinedException(name: Identifier) extends JediException("Undefined identifier: " + name) 
class TypeException(gripe: String = "Type Error") extends JediException(gripe)
class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error")
